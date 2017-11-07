{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (readFile)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (replace)

import System.Console.GetOpt

import System.Environment
import System.Directory
import System.FilePath
import Text.PrettyPrint.GenericPretty (pp, pretty, Out)
import Data.List (isInfixOf, intercalate, (\\))
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Data
import Data.Generics.Uniplate.Data
import Control.Monad

import Language.Fortran.ParserMonad (fromParseResult, FortranVersion(..), fromRight)

import Language.Fortran.Parser.FortranBigIron (bigIronParser, bigIronIncludeParser)

import Language.Fortran.AST
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Types
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Util.Position

import Data.Generics

import Text.Printf

main :: IO ()
main = do
  (opts, [path]) <- compileArgs =<< getArgs
  let version = FortranBigIron
  contents <- truncateLines version <$> flexReadFile path
  let toMain blks = ProgramFile (MetaInfo version path) [PUMain () (getSpan blks) Nothing blks Nothing]
  let prog = case takeExtension path of
        ".f"   -> fromRight . fromParseResult $ bigIronParser contents path
        ".inc" -> toMain . fromRight . fromParseResult $ bigIronIncludeParser contents path
        ".ins" -> toMain . fromRight . fromParseResult $ bigIronIncludeParser contents path
        ext    -> error $ "unknown file extension: " ++ ext
  let runInfer pf = analyseTypes . analyseRenames . initAnalysis $ pf
  mapM_ (printTag path (B.lines contents)) (allTags . fst $ runInfer prog)
  return ()

data Tag = Tag SrcSpan TagKind Name deriving (Eq, Show)

data TagKind
  = BlockData
  | CommonBlock
  | Function
  | Local
  | Program
  | Subroutine
  | Structure
  | Field
  | Global
  deriving (Eq, Show)

printTag :: FilePath -> [B.ByteString] -> Tag -> IO ()
printTag path lines (Tag (SrcSpan p1 p2) k n) =
  printf "%s\t%s\t/%s/;\" %s\tline:%d\n" n path (mkRegex lines p1 p2) (formatTagKind k) (posLine p1)

mkRegex :: [B.ByteString] -> Position -> Position -> String
mkRegex lines p1 p2
  | posLine p1 == posLine p2
  = "^" ++ escape (B.unpack (B.take (posColumn p2) line))
  | otherwise
  = "^" ++ escape (B.unpack line) ++ "$"
  where
  line = lines !! (posLine p1 - 1)
  escape = concatMap $ \case
             -- '^' -> "\\^"
             -- '.' -> "\\."
             -- '[' -> "\\["
             -- ']' -> "\\]"
             -- '$' -> "\\$"
             -- '(' -> "\\("
             -- ')' -> "\\)"
             -- '*' -> "\\*"
             -- '{' -> "\\{"
             -- '}' -> "\\}"
             -- '?' -> "\\?"
             -- '+' -> "\\+"
             -- '|' -> "\\|"
             '/' -> "\\/"
             c   -> [c]

formatTagKind :: TagKind -> String
formatTagKind k = case k of
  BlockData -> "block data"
  CommonBlock -> "common"
  Function -> "function"
  Local -> "local"
  Program -> "program"
  Subroutine -> "subroutine"
  Structure -> "type"
  Field -> "component"
  Global -> "variable"

allTags :: forall a. Data a => ProgramFile (Analysis a) -> [Tag]
allTags = everything (++) (mkQ [] (programUnitTags @a)
                           `extQ` (statementTags @a)
                           `extQ` (commonGroupTags @a)
                           `extQ` (structureItemTags @a)
                          )

programUnitTags :: ProgramUnit (Analysis a) -> [Tag]
programUnitTags pu = case pu of
  PUMain _ s (Just n) _ _ -> [Tag s Program n]
  PUSubroutine _ s _ n _ _ _ -> [Tag s Subroutine n]
  PUFunction _ s _ _ n _ _ _ _ -> [Tag s Function n]
  PUBlockData _ s (Just n) _ -> [Tag s BlockData n]
  _ -> []

statementTags :: Statement (Analysis a) -> [Tag]
statementTags st = case st of
  StDeclaration _ s _ _ ds -> concatMap declaratorTags (aStrip ds)
  StStructure _ s mn sis -> maybeToList (Tag s Structure <$> mn)
  StFunction _ _ (ExpValue _ s (ValVariable n)) _ _ -> [Tag s Function n]
  _ -> []

structureItemTags :: StructureItem (Analysis a) -> [Tag]
structureItemTags (StructFields _ _ _ _ ds)
  = concatMap (declaratorTagsWith Field) (aStrip ds)
structureItemTags _ = []

declaratorTags :: Declarator (Analysis a) -> [Tag]
declaratorTags = declaratorTagsWith Local

declaratorTagsWith :: TagKind -> Declarator (Analysis a) -> [Tag]
declaratorTagsWith k (DeclVariable _ _ name _ _)
  = maybeToList $ varTag k name
declaratorTagsWith k (DeclArray _ _ name _ _ _)
  = maybeToList $ varTag k name

-- dataGroupTags :: DataGroup (Analysis a) -> [Tag]
-- dataGroupTags (DataGroup _ _ mname names)
--   = undefined -- mapMaybe (varTag CommonBlock) (maybeToList mname) ++ mapMaybe (varTag Local) (aStrip names)

commonGroupTags :: CommonGroup (Analysis a) -> [Tag]
commonGroupTags (CommonGroup _ _ mname names)
  = mapMaybe (varTag CommonBlock) (maybeToList mname) ++ mapMaybe (varTag Local) (aStrip names)


varTag :: TagKind -> Expression (Analysis a) -> Maybe Tag
varTag k e = case e of
  ExpValue ann s (ValVariable n)
    | Local <- k
    , Just (IDType _ (Just CTExternal)) <- idType ann
      -> Nothing
    | otherwise
      -> Just (Tag s k n)
  ExpSubscript _ _ e' _ -> varTag k e'
  ExpDataRef _ _ e' _ -> varTag k e'
  _ -> Nothing

data Options = Options
  { includeDirs     :: [String] }

initOptions = Options []

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['I']
      ["include-dir"]
      (ReqArg (\ d opts -> opts { includeDirs = d:includeDirs opts }) "DIR")
      "directory to search for 'include' files"
  ]

compileArgs :: [ String ] -> IO (Options, [ String ])
compileArgs args =
  case getOpt Permute options args of
    (o, n, []) -> return (foldl (flip id) initOptions o, n)
    (_, _, errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where
    header = "Usage: fortran-tags [OPTION...] <file>"

flexReadFile :: String -> IO B.ByteString
flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile

truncateLines :: FortranVersion -> B.ByteString -> B.ByteString
truncateLines fv b
  | fv > FortranBigIron = b
  | otherwise           = B.unlines . map (B.filter (/='\r') . B.take 72) . B.lines $ b

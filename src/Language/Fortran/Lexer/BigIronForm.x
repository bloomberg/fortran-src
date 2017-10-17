-- -*- Mode: Haskell -*-
{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Fortran.Lexer.BigIronForm where

import Data.Word (Word8)
import Data.Char (toLower, ord)
import Data.List (isPrefixOf, any)
import Data.Maybe (fromJust, isNothing)
import Data.Data
import qualified Data.Bits
import qualified Data.ByteString.Char8 as B

import Control.Monad.State

import GHC.Generics

import Language.Fortran.ParserMonad

import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.Position

import Debug.Trace

}

$digit = [0-9]
$octalDigit = 0-7
$hexDigit = [a-f $digit]
$bit = 0-1

@binary = b\'$bit+\' | \'$bit+\'b
@octal = o\'$octalDigit+\' | \'$octalDigit+\'o
@hex = x\'$hexDigit+\' | \'$hexDigit+\'x | z\'$hexDigit+\' | \'$hexDigit+\'z

$letter = [a-z]
$alphanumeric = [$letter $digit]
$alphanumericExtended = [$letter $digit \_ \$]
$special = [\ \=\+\-\*\/\(\)\,\.\$]

-- This should really be 6 characters but there are many standard non-compliant
-- programs out there.
@idExtended = $letter $alphanumericExtended{0,9} $alphanumericExtended{0,9} $alphanumericExtended{0,9} $alphanumericExtended?
@id = $letter $alphanumeric{0,5}
@label = [0-9] $digit{0,4}

@idBI = $letter $alphanumericExtended*

@datatype = "integer" | "real" | "doubleprecision" | "complex" | "logical"
          -- BigIron extensions
          | "byte"

-- Numbers
@integerConst = $digit+ -- Integer constant
@posIntegerConst = [1-9] $digit*
@bozLiteralConst = (@binary|@octal|@hex)

-- For reals
@exponent = [ed] [\+\-]? @integerConst

-- For format items
@repeat = @posIntegerConst?
@width = @posIntegerConst

tokens :-

  <0> [c!\*d] / { commentP }                  { lexComment Nothing }
  "!" / { bangCommentP }                      { lexComment Nothing }
  <0> @label / { withinLabelColsP }           { addSpanAndMatch TLabel }
  <0> . / { \_ ai _ _ -> atColP 6 ai }        { toSC keyword }
  <0> " "                                     ;

  <0,st,keyword,iif> \n                       { resetPar >> toSC 0 >> addSpan TNewline }
  <0,st,keyword,iif> \r                       ;
  <0,st,keyword,iif> ";"                      { resetPar >> toSC 0 >> addSpan TNewline }

  <st,keyword> "("                            { addSpan TLeftPar }
  <iif> "("                                   { incPar >> addSpan TLeftPar }
  <st,keyword> ")"                            { typeSCChange >> addSpan TRightPar }
  <iif> ")"                                   { maybeToKeyword >> addSpan TRightPar }
  <st,iif> "(/" / { formatExtendedP }         { addSpan TLeftArrayPar }
  <st,iif> "/)" / { formatExtendedP }         { addSpan TRightArrayPar }
  <st,iif,keyword> ","                        { addSpan TComma }
  <st,iif,keyword> "."                        { addSpan TDot }
  <st,iif> ":" / { fortran77P }               { addSpan TColon }

  <keyword> @id / { idP }                     { toSC st >> addSpanAndMatch TId }
  <keyword> @idExtended / { extendedIdP }     { toSC st >> addSpanAndMatch TId }
  <keyword> @idBI / { extendedIdP }           { toSC st >> addSpanAndMatch TId }

  <keyword> "include" / { extended77P }       { toSC st >> addSpan TInclude }

  -- Tokens related to procedures and subprograms
  <keyword> "program"                         { toSC st >> addSpan TProgram }
  <keyword> "function" / { functionP }        { toSC st >> addSpan TFunction  }
  <keyword> "subroutine"                      { toSC st >> addSpan TSubroutine  }
  <keyword> "blockdata"                       { toSC st >> addSpan TBlockData  }
  <keyword> "structure"                       { toSC st >> addSpan TStructure  }
  <keyword> "union"                           { toSC st >> addSpan TUnion  }
  <keyword> "map"                             { toSC st >> addSpan TMap  }
  <keyword> "endstructure"                    { toSC st >> addSpan TEndStructure  }
  <keyword> "endunion"                        { toSC st >> addSpan TEndUnion  }
  <keyword> "endmap"                          { toSC st >> addSpan TEndMap  }
  <keyword> "record"                          { toSC st >> addSpan TRecord  }
  <keyword> "end"                             { toSC st >> addSpan TEnd  }

  -- Tokens related to assignment statements
-- <keyword> "assign"                          { toSC st >> addSpan TAssign  }
  <st,iif> "="                                { addSpan TOpAssign  }
--  <st> "to"                                   { addSpan TTo  }

  -- Tokens related to control statements
  <keyword> "goto"                            { toSC st >> addSpan TGoto  }
  <keyword> "if" / { ifP }                    { toSC iif >> addSpan TIf  }
  -- <st> "if" / { fortran77P }                  { toSC iif >> addSpan TIf  }
  <st,keyword> "then" / { fortran77P }        { toSC keyword >> addSpan TThen  }
  <keyword> "else" / {fortran77P }            { addSpan TElse  }
  <keyword> "elseif" / {fortran77P }          { toSC st >> addSpan TElsif  }
  <keyword> "endif" / {fortran77P }           { addSpan TEndif  }
  <keyword> "call"                            { toSC st >> addSpan TCall  }
  <keyword> "return"                          { toSC st >> addSpan TReturn  }
  <keyword> "save" / { fortran77P }           { toSC st >> addSpan TSave  }
  <keyword> "continue"                        { toSC st >> addSpan TContinue  }
  <keyword> "stop"                            { toSC st >> addSpan TStop  }
  <keyword> "exit" / { extended77P }          { toSC st >> addSpan TExit  }
  <keyword> "pause"                           { toSC st >> addSpan TPause  }
  <keyword> "do"                              { toSC st >> addSpan TDo }
  <keyword> "dowhile" / { extended77P }       { toSC st >> addSpan TDoWhile }
  <keyword> "enddo" / { extended77P }         { toSC st >> addSpan TEndDo  }

  -- Tokens related to I/O statements
  <keyword> "read"                            { toSC st >> addSpan TRead  }
  <keyword> "write"                           { toSC st >> addSpan TWrite  }
  <keyword> "rewind"                          { toSC st >> addSpan TRewind  }
  <keyword> "backspace"                       { toSC st >> addSpan TBackspace  }
  <keyword> "endfile"                         { toSC st >> addSpan TEndfile  }
  <keyword> "inquire" / { fortran77P }        { toSC st >> addSpan TInquire  }
  <keyword> "open" / { fortran77P }           { toSC st >> addSpan TOpen  }
  <keyword> "close" / { fortran77P }          { toSC st >> addSpan TClose  }
  <keyword> "print" / { fortran77P }          { toSC st >> addSpan TPrint  }
  <keyword> "type" / { bigIronP }             { toSC st >> addSpan TTypeBI  }

  -- Tokens related to non-executable statements

  -- Tokens related to speification statements
  <keyword> "dimension"                       { toSC st >> addSpan TDimension  }
  <keyword> "common"                          { toSC st >> addSpan TCommon  }
  <keyword> "equivalence"                     { toSC st >> addSpan TEquivalence  }
  <keyword> "external"                        { toSC st >> addSpan TExternal  }
  <keyword> "intrinsic" / { fortran77P }      { toSC st >> addSpan TIntrinsic  }
  <keyword> @datatype                         { typeSCChange >> addSpanAndMatch TType }
  <st> @datatype / { implicitStP }            { addSpanAndMatch TType }
  <keyword> "doublecomplex" / { extended77P } { typeSCChange >> addSpanAndMatch TType }
  <st> "doublecomplex" / { implicitTypeExtendedP }  { addSpanAndMatch TType }
  <keyword> "character" / { fortran77P }      { typeSCChange >> addSpanAndMatch TType }
  <st> "character" / { implicitType77P }      { addSpanAndMatch TType }
  <keyword> "implicit" / { fortran77P }       { toSC st >> addSpan TImplicit  }
  <st> "none" / { fortran77P }                { addSpan TNone  }
  <keyword> "parameter" / { fortran77P }      { toSC st >> addSpan TParameter  }
  <keyword> "entry" / { fortran77P }          { toSC st >> addSpan TEntry  }
  <keyword> "pointer" / { bigIronP }          { toSC st >> addSpan TPointer  }

  -- Tokens related to data initalization statement
  <keyword> "data"                            { toSC st >> addSpan TData  }

  <keyword> "automatic"                       { toSC st >> addSpan TAutomatic  }

  -- Tokens related to format statement
  <keyword> "format"                          { toSC st >> addSpan TFormat  }
  <st> "(".*")" / { formatP }                 { addSpanAndMatch TBlob }

  -- Tokens needed to parse integers, reals, double precision and complex
  -- constants
  <st,iif> @exponent / { exponentP }          { addSpanAndMatch TExponent }
  <st,iif> @integerConst                      { addSpanAndMatch TInt }
    -- can be part (end) of function type declaration
  <keyword> @integerConst                     { typeSCChange >> addSpanAndMatch TInt }
  <st,iif,keyword> @bozLiteralConst           { addSpanAndMatch TBozIntBI }

  -- String
  <st,iif> \' / { fortran77P }                { strAutomaton '\'' 0 }
  <st,iif> \" / { fortran77P }                { strAutomaton '"'  0 }

  -- Logicals
  <st,iif> (".true."|".false.")               { addSpanAndMatch TBool  }

  -- Arithmetic operators
  <st,iif> "+"                                { addSpan TOpPlus  }
  <st,iif> "-"                                { addSpan TOpMinus  }
  <st,iif> "**"                               { addSpan TOpExp  }
    -- can be part of function type declaration
  <st,iif,keyword> "*"                        { addSpan TStar  }
  <st,iif> "/"                                { addSpan TSlash  }

  -- Logical operators
  <st,iif> ".or."                             { addSpan TOpOr  }
  <st,iif> ".and."                            { addSpan TOpAnd  }
  <st,iif> ".not."                            { addSpan TOpNot  }
  <st,iif> ".eqv." / { fortran77P }           { addSpan TOpEquivalent  }
  <st,iif> ".neqv." / { fortran77P }          { addSpan TOpNotEquivalent  }

  -- Relational operators
  <st,iif> "<" / { extended77P }              { addSpan TOpLT  }
  <st,iif> "<=" / { extended77P }             { addSpan TOpLE  }
  <st,iif> "==" / { extended77P }             { addSpan TOpEQ  }
  -- <st,iif> "!=" / { extended77P }             { addSpan TOpNE  }
  <st,iif> ">" / { extended77P }              { addSpan TOpGT  }
  <st,iif> ">=" / { extended77P }             { addSpan TOpGE  }
  <st,iif> ".lt."                             { addSpan TOpLT  }
  <st,iif> ".le."                             { addSpan TOpLE  }
  <st,iif> ".eq."                             { addSpan TOpEQ  }
  <st,iif> ".ne."                             { addSpan TOpNE  }
  <st,iif> ".gt."                             { addSpan TOpGT  }
  <st,iif> ".ge."                             { addSpan TOpGE  }

  <st,iif> "%val" / { bigIronP }              { addSpan TByVal  }

  -- ID
  <st,iif> @id                                { addSpanAndMatch TId }
  <st,iif> @idExtended / { extended77P }      { addSpanAndMatch TId }
  <st,iif> @idBI / { bigIronP }               { addSpanAndMatch TId }

  -- Strings
  <st> @posIntegerConst "h" / { fortran66P }  { lexHollerith }
  <st,iif> @posIntegerConst "h" / { bigIronP }    { lexHollerith }

{

--------------------------------------------------------------------------------
-- Predicated lexer helpers
--------------------------------------------------------------------------------

formatP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
formatP _ _ _ ai
  | Just TFormat{} <- aiPreviousToken ai = True
  | otherwise = False

formatExtendedP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
formatExtendedP fv _ _ ai = fv `elem` [Fortran77Extended, FortranBigIron] &&
  case xs of
    [ TFormat _, _ ] -> False
    [ TLabel _ _, TFormat _ ] -> False
    _ -> True
  where
    xs = take 2 . reverse . aiPreviousTokensInLine $ ai

implicitType77P :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
implicitType77P fv b c d = fortran77P fv b c d && implicitStP fv b c d

implicitTypeExtendedP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
implicitTypeExtendedP fv b c d = extended77P fv b c d && implicitStP fv b c d

implicitStP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
implicitStP fv _ _ ai = checkPreviousTokensInLine f ai
  where
    f (TImplicit _) = True
    f _ = False

extendedIdP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
extendedIdP fv a b ai = fv `elem` [Fortran77Extended, FortranBigIron] && idP fv a b ai

idP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
idP fv ao i ai = not (doP fv ai) && not (ifP fv ao i ai)
             && (equalFollowsP fv ai || rParFollowsP fv ai)

doP :: FortranVersion -> AlexInput -> Bool
doP fv ai = "do" `isPrefixOf` (reverse . lexemeMatch . aiLexeme $ ai) &&
    case unParse (lexer $ f 0) ps of
      ParseOk True _ -> True
      _ -> False
  where
    ps = ParseState
      { psAlexInput = ai { aiStartCode = st}
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = ParanthesesCount 0 False
      , psContext = [ ConStart ] }
    f 0 t =
      case t of
        TNewline{} -> return False
        TEOF{} -> return False
        TLeftPar{} -> lexer $ f 1
        TComma{} -> return True
        _ -> lexer $ f 0
    f !n t =
      case t of
        TLeftPar{} -> lexer $ f (n+1)
        TRightPar{} -> lexer $ f (n-1)
        _ -> lexer $ f n

ifP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
ifP fv _ _ ai = "if" == (reverse . lexemeMatch . aiLexeme $ ai) &&
    case unParse (lexer $ f) ps of
      ParseOk True _ -> True
      _ -> False
  where
    ps = ParseState
      { psAlexInput = ai { aiStartCode = st}
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = ParanthesesCount 0 False
      , psContext = [ ConStart ] }
    f t =
      case t of
        -- IF is always followed by (
        TLeftPar{} -> return True
        _ -> return False

functionP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
functionP fv _ _ ai = "function" == (reverse . lexemeMatch . aiLexeme $ ai) &&
    case unParse (lexer $ f) ps of
      ParseOk True _ -> True
      _ -> False
  where
    ps = ParseState
      { psAlexInput = ai { aiStartCode = st}
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = ParanthesesCount 0 False
      , psContext = [ ConStart ] }
    f t =
      case t of
        -- a function keyword should be followed by the name and a left paren
        TId{} -> lexer f
        TLeftPar{} -> return True
        _ -> return False

equalFollowsP :: FortranVersion -> AlexInput -> Bool
equalFollowsP fv ai =
    case unParse (lexer $ f False 0) ps of
      ParseOk True _ -> True
      _ -> False
  where
    ps = ParseState
      { psAlexInput = ai { aiStartCode = st}
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = ParanthesesCount 0 False
      , psContext = [ ConStart ] }
    f False 0 t =
      case t of
        TNewline{} -> return False
        TEOF{} -> return False
        TOpAssign{} -> return True
        TLeftPar{} -> lexer $ f True 1
        TDot{} -> lexer $ f False 0
        TId{} -> lexer $ f False 0
        _ -> return False
    f True 0 t =
      case t of
        TOpAssign{} -> return True
        TDot{} -> lexer $ f True 0
        TId{} -> lexer $ f True 0
        TLeftPar{} -> lexer $ f True 1
        _ -> return False
    f True n t =
      case t of
        TNewline{} -> return False
        TEOF{} -> return False
        TLeftPar{} -> lexer $ f True (n + 1)
        TRightPar{} -> lexer $ f True (n - 1)
        _ -> lexer $ f True n

rParFollowsP :: FortranVersion -> AlexInput -> Bool
rParFollowsP fv ai =
    case unParse (lexer $ f) ps of
      ParseOk True _ -> True
      _ -> False
  where
    ps = ParseState
      { psAlexInput = ai { aiStartCode = st}
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = ParanthesesCount 0 False
      , psContext = [ ConStart ] }
    f t =
      case t of
        TRightPar{} -> return True
        _ -> return False

commentP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
commentP _ aiOld _ aiNew = atColP 1 aiOld && _endsWithLine
  where
    _endsWithLine = (posColumn . aiPosition) aiNew /= 1

bangCommentP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
bangCommentP _ aiOld _ aiNew = _endsWithLine
  where
    _endsWithLine = (posColumn . aiPosition) aiNew /= 1

withinLabelColsP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
withinLabelColsP _ aiOld _ aiNew = getCol aiOld >= 1 && getCol aiNew <= 6
  where
    getCol = posColumn . aiPosition

atColP :: Int -> AlexInput -> Bool
atColP n ai = (posColumn . aiPosition) ai == n

-- This predicate allows to distinguish identifiers and real exponent tokens
-- by looking at previous token. Since exponent can only follow a "." or an
-- integer token. Anything other previous token will prevent matching the input
-- as an exponent token.
exponentP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
exponentP _ _ _ ai =
  case aiPreviousTokensInLine ai of
    -- real*8 d8 is not an exponent
    TInt{} : TStar{} : TType{} : _ -> False
    TInt{} : _ -> True
    TDot{} : _ -> True
    _ -> False

fortran66P :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
fortran66P fv _ _ _ = fv == Fortran66

fortran77P :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
fortran77P fv _ _ _ = fv == Fortran77 || fv == Fortran77Extended || fv == FortranBigIron

extended77P :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
extended77P fv _ _ _ = fv == Fortran77Extended || fv == FortranBigIron

bigIronP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
bigIronP fv _ _ _ = fv == FortranBigIron

--------------------------------------------------------------------------------
-- Lexer helpers
--------------------------------------------------------------------------------

addSpan :: (SrcSpan -> Token) -> LexAction (Maybe Token)
addSpan cons = do
  s <- getLexemeSpan
  return $ Just $ cons s

addSpanAndMatch :: (SrcSpan -> String -> Token) -> LexAction (Maybe Token)
addSpanAndMatch cons = do
  s <- getLexemeSpan
  m <- getMatch
  return $ Just $ cons s m

getLexeme :: LexAction Lexeme
getLexeme = do
  ai <- getAlex
  return $ aiLexeme ai

putLexeme :: Lexeme -> LexAction ()
putLexeme lexeme = do
  ai <- getAlex
  putAlex $ ai { aiLexeme = lexeme }

resetLexeme :: LexAction ()
resetLexeme = putLexeme initLexeme

getMatch :: LexAction String
getMatch = do
  lexeme <- getLexeme
  return $ (reverse . lexemeMatch) lexeme

putMatch :: String -> LexAction ()
putMatch newMatch = do
  lexeme <- getLexeme
  putLexeme $ lexeme { lexemeMatch = reverse newMatch }

incWhiteSensitiveCharCount :: LexAction ()
incWhiteSensitiveCharCount = do
  ai <- getAlex
  let wsc = aiWhiteSensitiveCharCount ai
  putAlex $ ai { aiWhiteSensitiveCharCount = wsc + 1 }

resetWhiteSensitiveCharCount :: LexAction ()
resetWhiteSensitiveCharCount = do
  ai <- getAlex
  putAlex $ ai { aiWhiteSensitiveCharCount = 0 }

setCaseSensitive :: LexAction ()
setCaseSensitive = do
  ai <- getAlex
  putAlex $ ai { aiCaseSensitive = True }

setCaseInsensitive :: LexAction ()
setCaseInsensitive = do
  ai <- getAlex
  putAlex $ ai { aiCaseSensitive = False }

instance Spanned Lexeme where
  getSpan lexeme =
    let ms = lexemeStart lexeme
        me = lexemeEnd lexeme in
      SrcSpan (fromJust ms) (fromJust me)
  setSpan _ = error "Lexeme span cannot be set."

updatePreviousToken :: Maybe Token -> LexAction ()
updatePreviousToken maybeToken = do
  ai <- getAlex
  putAlex $ ai { aiPreviousToken = maybeToken }

addToPreviousTokensInLine :: Token -> LexAction ()
addToPreviousTokensInLine token = do
  ai <- getAlex
  putAlex $
    case token of
      TNewline _ -> updatePrevTokens ai [ ]
      t -> updatePrevTokens ai $ t : aiPreviousTokensInLine ai
  where
    updatePrevTokens ai tokens = ai { aiPreviousTokensInLine = tokens }

checkPreviousTokensInLine :: (Token -> Bool) -> AlexInput -> Bool
checkPreviousTokensInLine prop ai = any prop $ aiPreviousTokensInLine ai

getLexemeSpan :: LexAction SrcSpan
getLexemeSpan = do
  lexeme <- getLexeme
  return $ getSpan lexeme

-- With the existing alexGetByte implementation comments are matched without
-- whitespace characters. However, we have access to final column number,
-- we know the comment would start at column, and we have access to the absolute
-- offset so instead of using match, lexComment takes a slice from the original
-- source input
lexComment :: Maybe Char -> LexAction (Maybe Token)
lexComment mc = do
  m <- getMatch
  s <- getLexemeSpan
  alex <- getAlex
  let modifiedAlex = alex { aiWhiteSensitiveCharCount = 1 }
  case mc of
    Just '\n' -> return Nothing --- $ Just $ TComment s $ tail m
    Just _ ->
      case alexGetByte modifiedAlex of
        Just (w, _) | fromIntegral w == ord '\n' -> do
          return Nothing --- $ Just $ TComment s $ tail m
        Just (_, newAlex) -> do
          putAlex newAlex
          lexComment Nothing
        Nothing -> fail "Comment abruptly ended."
    Nothing ->
      case alexGetByte modifiedAlex of
        Just (_, newAlex) -> lexComment (Just $ (head . lexemeMatch . aiLexeme) newAlex)
        Nothing -> return Nothing --- $ Just $ TComment s $ tail m


{-
     Chars
      +-+
      | |
      | |
      | v
      +-+  Nothing  +-+
+---> |0|---------->+3|
  +-> +++           +-+
  |    |
' |    | '
  |    v
  |   +++  Nothing  +-+
  +---|1|----------->2|
      +++           +++
       |             ^
       +-------------+
            Chars
-}
strAutomaton :: Char -> Int -> LexAction (Maybe Token)
strAutomaton c 0 = do
  setCaseSensitive
  incWhiteSensitiveCharCount
  alex <- getAlex
  case alexGetByte alex of
    Just (_, newAlex) -> do
      putAlex newAlex
      m <- getMatch
      if last m == c
      then strAutomaton c 1
      else strAutomaton c 0
    Nothing -> strAutomaton c 3
strAutomaton c 1 = do
  incWhiteSensitiveCharCount
  alex <- getAlex
  case alexGetByte alex of
    Just (_, newAlex) -> do
      let m = lexemeMatch . aiLexeme $ newAlex
      if head m == c
      then do
        putAlex newAlex
        putMatch $ reverse . tail $ m
        strAutomaton c 0
      else strAutomaton c 2
    Nothing -> strAutomaton c 2
strAutomaton c 2 = do
  s <- getLexemeSpan
  m <- getMatch
  resetWhiteSensitiveCharCount
  setCaseInsensitive
  return $ Just $ TString s $ (init . tail) m
strAutomaton c 3 = fail "Unmatched string."

lexHollerith :: LexAction (Maybe Token)
lexHollerith = do
  match' <- getMatch
  let len = read $ init match' -- Get n of "nH" from string
  putMatch ""
  ai <- getAlex
  putAlex $ ai { aiWhiteSensitiveCharCount = len }
  lexed <- lexN len
  s <- getLexemeSpan
  return $ do
    hollerith <- lexed
    return $ THollerith s hollerith

lexN :: Int -> LexAction (Maybe String)
lexN n = do
  alex <- getAlex
  match' <- getMatch
  let len = length match'
  if n == len
  then return $ Just match'
  else
    case alexGetByte alex of
      Just (_, newAlex) -> do
        putAlex newAlex
        lexN n
      Nothing -> return Nothing

maybeToKeyword :: LexAction (Maybe Token)
maybeToKeyword = do
  decPar
  pcActual <- pcActual . psParanthesesCount <$> get
  if pcActual == 0
  then toSC keyword
  else return Nothing

typeSCChange :: LexAction (Maybe Token)
typeSCChange = do
  ps <- get
  let hypotheticalPs = ps { psAlexInput = (psAlexInput ps) { aiStartCode = keyword } }
  let isFunction = case unParse (lexer f) hypotheticalPs of { ParseOk True _ -> True; _ -> False }
  if isFunction
  then return Nothing
  else toSC st
  where
    f TFunction{} = return True
      -- can be part of function type declaration
    f TStar{} = return True
    f _ = return False

toSC :: Int -> LexAction (Maybe Token)
toSC startCode = do
  ai <- getAlex
  if startCode == 0
  then putAlex $ ai { aiStartCode = startCode, aiWhiteSensitiveCharCount = 6 }
  else putAlex $ ai { aiStartCode = startCode }
  return Nothing

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

data Token = TLeftPar             SrcSpan
           | TRightPar            SrcSpan
           | TLeftArrayPar        SrcSpan
           | TRightArrayPar       SrcSpan
           | TComma               SrcSpan
           | TDot                 SrcSpan
           | TColon               SrcSpan
           | TInclude             SrcSpan
           | TProgram             SrcSpan
           | TFunction            SrcSpan
           | TSubroutine          SrcSpan
           | TBlockData           SrcSpan
           | TStructure           SrcSpan
           | TRecord              SrcSpan
           | TUnion               SrcSpan
           | TMap                 SrcSpan
           | TEndStructure        SrcSpan
           | TEndUnion            SrcSpan
           | TEndMap              SrcSpan
           | TEnd                 SrcSpan
           | TAssign              SrcSpan
           | TOpAssign            SrcSpan
           | TTo                  SrcSpan
           | TGoto                SrcSpan
           | TIf                  SrcSpan
           | TThen                SrcSpan
           | TElse                SrcSpan
           | TElsif               SrcSpan
           | TEndif               SrcSpan
           | TCall                SrcSpan
           | TReturn              SrcSpan
           | TSave                SrcSpan
           | TContinue            SrcSpan
           | TStop                SrcSpan
           | TExit                SrcSpan
           | TPause               SrcSpan
           | TDo                  SrcSpan
           | TDoWhile             SrcSpan
           | TEndDo               SrcSpan
           | TRead                SrcSpan
           | TWrite               SrcSpan
           | TRewind              SrcSpan
           | TBackspace           SrcSpan
           | TEndfile             SrcSpan
           | TInquire             SrcSpan
           | TOpen                SrcSpan
           | TClose               SrcSpan
           | TPrint               SrcSpan
           | TTypeBI              SrcSpan
           | TDimension           SrcSpan
           | TCommon              SrcSpan
           | TEquivalence         SrcSpan
           | TPointer             SrcSpan
           | TExternal            SrcSpan
           | TIntrinsic           SrcSpan
           | TType                SrcSpan String
           | TEntry               SrcSpan
           | TImplicit            SrcSpan
           | TNone                SrcSpan
           | TParameter           SrcSpan
           | TData                SrcSpan
           | TAutomatic           SrcSpan
           | TFormat              SrcSpan
           | TBlob                SrcSpan String
           | TInt                 SrcSpan String
           | TBozIntBI            SrcSpan String
           | TExponent            SrcSpan String
           | TBool                SrcSpan String
           | TByVal               SrcSpan
           | TOpPlus              SrcSpan
           | TOpMinus             SrcSpan
           | TOpExp               SrcSpan
           | TStar                SrcSpan
           | TSlash               SrcSpan
           | TOpOr                SrcSpan
           | TOpAnd               SrcSpan
           | TOpNot               SrcSpan
           | TOpEquivalent        SrcSpan
           | TOpNotEquivalent     SrcSpan
           | TOpLT                SrcSpan
           | TOpLE                SrcSpan
           | TOpEQ                SrcSpan
           | TOpNE                SrcSpan
           | TOpGT                SrcSpan
           | TOpGE                SrcSpan
           | TId                  SrcSpan String
           | TComment             SrcSpan String
           | TString              SrcSpan String
           | THollerith           SrcSpan String
           | TLabel               SrcSpan String
           | TNewline             SrcSpan
           | TEOF                 SrcSpan
           deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance FirstParameter Token SrcSpan
instance FirstParameter Token SrcSpan => Spanned Token where
  getSpan a = getFirstParameter a
  setSpan e a = setFirstParameter e a

instance Tok Token where
  eofToken (TEOF _) = True
  eofToken _ = False

--------------------------------------------------------------------------------
-- AlexInput & related definitions
--------------------------------------------------------------------------------

data Lexeme = Lexeme
  { lexemeMatch :: String
  , lexemeStart :: Maybe Position
  , lexemeEnd   :: Maybe Position
  } deriving (Show)

initLexeme :: Lexeme
initLexeme = Lexeme
  { lexemeMatch = ""
  , lexemeStart = Nothing
  , lexemeEnd   = Nothing }

data AlexInput = AlexInput
  { aiSourceBytes               :: B.ByteString
  , aiEndOffset                 :: Int
  , aiPosition                  :: Position
  , aiBytes                     :: [Word8]
  , aiPreviousChar              :: Char
  , aiLexeme                    :: Lexeme
  , aiWhiteSensitiveCharCount   :: Int
  , aiStartCode                 :: Int
  , aiPreviousToken             :: Maybe Token
  , aiPreviousTokensInLine      :: [ Token ]
  , aiCaseSensitive             :: Bool
  } deriving (Show)

instance Loc AlexInput where
  getPos = aiPosition

instance LastToken AlexInput Token where
  getLastToken = aiPreviousToken

type LexAction a = Parse AlexInput Token a

vanillaAlexInput :: AlexInput
vanillaAlexInput = AlexInput
  { aiSourceBytes = B.empty
  , aiEndOffset = 0
  , aiPosition = initPosition
  , aiBytes = []
  , aiPreviousChar = '\n'
  , aiLexeme = initLexeme
  , aiWhiteSensitiveCharCount = 6
  , aiStartCode = 0
  , aiPreviousToken = Nothing
  , aiPreviousTokensInLine = [ ]
  , aiCaseSensitive = False
  }

updateLexeme :: Maybe Char -> Position -> AlexInput -> AlexInput
updateLexeme maybeChar p ai =
  let lexeme = aiLexeme ai
      match = lexemeMatch lexeme
      newMatch =
        case maybeChar of
          Just c -> c : match
          Nothing -> match
      start = lexemeStart lexeme
      newStart = if isNothing start then Just p else start
      newEnd = Just p in
    ai { aiLexeme = Lexeme newMatch newStart newEnd }

--------------------------------------------------------------------------------
-- Definitions needed for alexScanUser
--------------------------------------------------------------------------------

data Move = Continuation | Char | Newline | NewlineComment

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai
  -- The process of reading individual bytes of the character
  | _bytes /= [] = Just (head _bytes, ai { aiBytes = tail _bytes })
  -- When all characters are already read
  | posAbsoluteOffset _position == aiEndOffset ai = Nothing
  -- Skip the continuation line altogether
  | _isWhiteInsensitive && isContinuation ai = skip Continuation ai
  -- Skip the newline before a comment
  | _isWhiteInsensitive && isNewlineComment ai = skip NewlineComment ai
  -- If we are not parsing a Hollerith skip whitespace
  | _isWhiteInsensitive && _curChar `elem` [ ' ', '\t' ] = skip Char ai
  -- Read genuine character and advance. Also covers white sensitivity.
  | otherwise =
      let (_b:_bs) = utf8Encode _curChar in
        Just(_b, updateLexeme (Just _curChar) _position
          ai {
            aiPosition =
              case _curChar of
                '\n'  -> advance Newline ai
                _     -> advance Char ai,
            aiBytes = _bs,
            aiPreviousChar = _curChar,
            aiWhiteSensitiveCharCount =
              if _isWhiteInsensitive
              then 0
              else aiWhiteSensitiveCharCount ai - 1
          })
  where
    _curChar = (if aiCaseSensitive ai then id else toLower) $ currentChar ai
    _bytes = aiBytes ai
    _position = aiPosition ai
    _isWhiteInsensitive = aiWhiteSensitiveCharCount ai == 0

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai = aiPreviousChar ai

takeNChars :: Integer -> AlexInput -> String
takeNChars n ai =
  B.unpack . B.take (fromIntegral n) . B.drop (fromIntegral _dropN) $ aiSourceBytes ai
  where
    _dropN = posAbsoluteOffset . aiPosition $ ai

currentChar :: AlexInput -> Char
currentChar ai = B.index (aiSourceBytes ai) (fromIntegral . posAbsoluteOffset . aiPosition $ ai)

isContinuation :: AlexInput -> Bool
isContinuation ai =
  take 6 _next7 == "\n     " && not (last _next7 `elem` [' ', '0', '\n', '\r'])
  where
    _next7 = takeNChars 7 ai

isNewlineComment :: AlexInput -> Bool
isNewlineComment ai =
  _next1 == "\n" && isCommentLine ai p
  where
    _next1 = takeNChars 1 ai
    p = (aiPosition ai) { posAbsoluteOffset = posAbsoluteOffset (aiPosition ai) + 1 }

skip :: Move -> AlexInput -> Maybe (Word8, AlexInput)
skip move ai =
  let _newPosition = advance move ai in
    alexGetByte $ updateLexeme Nothing _newPosition $ ai { aiPosition = _newPosition }

advance :: Move -> AlexInput -> Position
advance move ai =
  case move of
    Char ->
      position { posAbsoluteOffset = _absl + 1, posColumn = _col + 1 }
    Continuation ->
      position { posAbsoluteOffset = _absl + 7, posColumn = 7, posLine = _line + 1 }
    Newline ->
      position { posAbsoluteOffset = _absl + 1, posColumn = 1, posLine = _line + 1 }
    NewlineComment -> --- traceShowId $
      skipCommentLines ai
        position { posAbsoluteOffset = _absl + 1, posColumn = 1, posLine = _line + 1 }
  where
    position = aiPosition ai
    _col = posColumn position
    _line = posLine position
    _absl = posAbsoluteOffset position


skipCommentLines :: AlexInput -> Position -> Position
skipCommentLines ai p = go p p
  where
  go p' p
    --- | traceShow (p, line) False = undefined
    -- eof is not a comment line
    | not (null line)
    , isCommentLine ai p
    = go p p{ posAbsoluteOffset = posAbsoluteOffset p + length line + 1
            , posColumn = 1, posLine = posLine p + 1
            }
    | isContinuation ai'
    = advance Continuation ai'
    | otherwise
      -- after skipping comment lines, place cursor right at the last newline
    = p2
    where
    line = takeLine p ai
    line' = takeLine p' ai
    p2 = p' { posAbsoluteOffset = posAbsoluteOffset p' + length line'
            , posColumn = length line'
            }
    ai' = ai { aiPosition = p2 }

isCommentLine :: AlexInput -> Position -> Bool
isCommentLine ai p
    | map toLower (take 1 line) `elem` ["c", "d", "!", "*"]
      || all (`elem` " \t") line
      || head (dropWhile (`elem` " \t") line) == '!'
    = True
    | otherwise
    = False
    where
    line = takeLine p ai

takeLine :: Position -> AlexInput -> String
takeLine p ai =
  B.unpack . B.takeWhile (/='\n') . B.drop (fromIntegral _dropN) $ aiSourceBytes ai
  where
    _dropN = posAbsoluteOffset p

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . _go . ord
  where
    _go oc
      | oc <= 0x7f   = [oc]
      | oc <= 0x7ff  = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                       , 0x80 + oc Data.Bits..&. 0x3f
                       ]
      | oc <= 0xffff = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                       , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                       , 0x80 + oc Data.Bits..&. 0x3f
                       ]
      | otherwise    = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                       , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                       , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                       , 0x80 + oc Data.Bits..&. 0x3f
                       ]

--------------------------------------------------------------------------------
-- Lexer definition
--------------------------------------------------------------------------------

lexer :: (Token -> LexAction a) -> LexAction a
lexer cont = cont =<< lexer'

lexer' :: LexAction Token
lexer' = do
  resetLexeme
  alexInput <- getAlex
  let startCode = aiStartCode alexInput
  version <- getVersion
  case alexScanUser version alexInput startCode of
    AlexEOF -> return $ TEOF $ SrcSpan (getPos alexInput) (getPos alexInput)
    AlexError _ -> do
      parseState <- get
      fail $ psFilename parseState ++ ": lexing failed. "
    AlexSkip newAlex _ -> putAlex newAlex >> lexer'
    AlexToken newAlex startCode action -> do
      putAlex newAlex
      maybeToken <- action
      case maybeToken of
        Just token -> do
          updatePreviousToken maybeToken
          addToPreviousTokensInLine token
          return token
        Nothing -> lexer'

alexScanUser :: FortranVersion -> AlexInput -> Int -> AlexReturn (LexAction (Maybe Token))

--------------------------------------------------------------------------------
-- Functions to help testing & output
--------------------------------------------------------------------------------

initParseState :: B.ByteString -> FortranVersion -> String -> ParseState AlexInput
initParseState srcBytes fortranVersion filename =
  _vanillaParseState { psAlexInput = _vanillaAlexInput }
  where
    _vanillaParseState = ParseState
      { psAlexInput = undefined
      , psVersion = fortranVersion
      , psFilename = filename
      , psParanthesesCount = ParanthesesCount 0 False
      , psContext = [ ConStart ] }
    _vanillaAlexInput = vanillaAlexInput
      { aiSourceBytes = srcBytes
      , aiEndOffset   = fromIntegral $ B.length srcBytes  }

collectFixedTokens :: FortranVersion -> B.ByteString -> [Token]
collectFixedTokens version srcInput =
    collectTokens lexer' $ initParseState srcInput version "<unknown>"

collectFixedTokensSafe :: FortranVersion -> B.ByteString -> Maybe [Token]
collectFixedTokensSafe version srcInput =
    collectTokensSafe lexer' $ initParseState srcInput version "<unknown>"

}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving, DeriveGeneric, TupleSections #-}

-- |
-- Common data structures and functions supporting analysis of the AST.
module Language.Fortran.Analysis
  ( initAnalysis, stripAnalysis, Analysis(..)
  , varName, srcName, lvVarName, lvSrcName, isNamedExpression
  , genVar, puName, puSrcName, blockRhsExprs, rhsExprs
  , ModEnv, NameType(..), IDType(..), ConstructType(..), BaseType(..)
  , lhsExprs, isLExpr, allVars, analyseAllLhsVars, analyseAllLhsVars1, allLhsVars
  , blockVarUses, blockVarDefs
  , BB, BBGr
  , TransFunc, TransFuncM )
where

import Language.Fortran.Util.Position (SrcSpan)
import Data.Generics.Uniplate.Data
import Data.Data
import Language.Fortran.AST
import Language.Fortran.LValue
import Data.Graph.Inductive.PatriciaTree (Gr)
import GHC.Generics (Generic)
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Binary
import Language.Fortran.Intrinsics (getIntrinsicDefsUses, allIntrinsics)
import Data.Bifunctor (first)

--------------------------------------------------

-- | Basic block
type BB a = [Block a]

-- | Basic block graph.
type BBGr a = Gr (BB a) ()

-- Allow graphs to reside inside of annotations
deriving instance (Typeable a, Typeable b) => Typeable (Gr a b)
instance (Typeable a, Typeable b) => Data (Gr a b) where
    gfoldl _k z v = z v -- make graphs opaque to Uniplate
    toConstr _    = error "toConstr"
    gunfold _ _   = error "gunfold"
    dataTypeOf _  = mkNoRepType "Gr"

--------------------------------------------------

-- | The type of "transformBi"-family functions
type TransFunc f g a = (f (Analysis a) -> f (Analysis a)) -> g (Analysis a) -> g (Analysis a)
-- | The type of "transformBiM"-family functions
type TransFuncM m f g a = (f (Analysis a) -> m (f (Analysis a))) -> g (Analysis a) -> m (g (Analysis a))

-- Describe a Fortran name as either a program unit or a variable.
data NameType = NTSubprogram | NTVariable | NTIntrinsic deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance Binary NameType
instance Out NameType

-- Module environments are associations between source name and
-- (unique name, name type) in a specific module.
type ModEnv = M.Map String (String, NameType)

-- Fortran data types are broken down into 'construct type' and 'base type'.
data ConstructType =
    CTFunction
  | CTSubroutine
  | CTExternal
  | CTVariable
  | CTArray
  | CTParameter
  | CTIntrinsic
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Out ConstructType
instance Binary ConstructType

data IDType = IDType
  { idVType :: Maybe BaseType
  , idCType :: Maybe ConstructType }
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Out IDType
instance Binary IDType

data Analysis a = Analysis
  { prevAnnotation :: a -- ^ original annotation
  , uniqueName     :: Maybe String -- ^ unique name for function/variable, after variable renaming phase
  , sourceName     :: Maybe String -- ^ original name for function/variable found in source text
  , bBlocks        :: Maybe (BBGr (Analysis a)) -- ^ basic block graph
  , insLabel       :: Maybe Int -- ^ unique number for each block during dataflow analysis
  , moduleEnv      :: Maybe ModEnv
  , idType         :: Maybe IDType
  , allLhsVarsAnn  :: [Name]
  }
  deriving (Data, Show, Eq, Generic)

instance Functor Analysis where
  fmap f analysis =
    Analysis
    { prevAnnotation = f (prevAnnotation analysis)
    , uniqueName = uniqueName analysis
    , sourceName = sourceName analysis
    , bBlocks = fmap (first . fmap . fmap . fmap $ f) . bBlocks $ analysis
    , insLabel = insLabel analysis
    , moduleEnv = moduleEnv analysis
    , idType = idType analysis
    , allLhsVarsAnn = allLhsVarsAnn analysis
    }

instance Out (Analysis a) where
  doc a = parens . text . unwords . map (uncurry (++) . fmap fromJust) . filter (isJust . snd) $
            [ ("uniqueName: ", uniqueName a)
            , ("sourceName: ", sourceName a)
            , ("insLabel: ", fmap show (insLabel a))
            , ("idType: ", fmap show (idType a)) ]
  docPrec _ = doc

analysis0 a = Analysis { prevAnnotation = a
                       , uniqueName     = Nothing
                       , sourceName     = Nothing
                       , bBlocks        = Nothing
                       , insLabel       = Nothing
                       , moduleEnv      = Nothing
                       , idType         = Nothing
                       , allLhsVarsAnn  = [] }

-- | True iff the expression can be used with varName or srcName
isNamedExpression :: Expression a -> Bool
isNamedExpression (ExpValue _ _ (ValVariable _))  = True
isNamedExpression (ExpValue _ _ (ValIntrinsic _)) = True
isNamedExpression _                               = False

-- | Obtain either uniqueName or source name from an ExpValue variable.
varName :: Expression (Analysis a) -> String
varName (ExpValue (Analysis { uniqueName = Just n }) _ (ValVariable {}))  = n
varName (ExpValue (Analysis { sourceName = Just n }) _ (ValVariable {}))  = n
varName (ExpValue _ _ (ValVariable n))                                    = n
varName (ExpValue (Analysis { uniqueName = Just n }) _ (ValIntrinsic {})) = n
varName (ExpValue (Analysis { sourceName = Just n }) _ (ValIntrinsic {})) = n
varName (ExpValue _ _ (ValIntrinsic n))                                   = n
varName _                                                                 = error "Use of varName on non-variable."

-- | Obtain the source name from an ExpValue variable.
srcName :: Expression (Analysis a) -> String
srcName (ExpValue (Analysis { sourceName = Just n }) _ (ValVariable {}))  = n
srcName (ExpValue _ _ (ValVariable n))                                    = n
srcName (ExpValue (Analysis { sourceName = Just n }) _ (ValIntrinsic {})) = n
srcName (ExpValue _ _ (ValIntrinsic n))                                   = n
srcName _                                                                 = error "Use of srcName on non-variable."

-- | Obtain either uniqueName or source name from an LvSimpleVar variable.
lvVarName :: LValue (Analysis a) -> String
lvVarName (LvSimpleVar (Analysis { uniqueName = Just n }) _ _)  = n
lvVarName (LvSimpleVar (Analysis { sourceName = Just n }) _ _)  = n
lvVarName (LvSimpleVar _ _ n)                                   = n
lvVarName _                                                     = error "Use of lvVarName on non-variable."

-- | Obtain the source name from an LvSimpleVar variable.
lvSrcName :: LValue (Analysis a) -> String
lvSrcName (LvSimpleVar (Analysis { sourceName = Just n }) _ _) = n
lvSrcName (LvSimpleVar _ _ n) = n
lvSrcName _ = error "Use of lvSrcName on a non-variable"

-- | Generate an ExpValue variable with its source name == to its uniqueName.
genVar :: Analysis a -> SrcSpan -> String -> Expression (Analysis a)
genVar a s n = ExpValue (a { uniqueName = Just n, sourceName = Just n }) s v
  where
    v | Just CTIntrinsic <- idCType =<< idType a = ValIntrinsic n
      | otherwise                                = ValVariable n

-- | Obtain either ProgramUnit uniqueName or whatever is in the AST.
puName :: ProgramUnit (Analysis a) -> ProgramUnitName
puName pu
  | Just n <- uniqueName (getAnnotation pu) = Named n
  | otherwise                               = getName pu

-- | Obtain either ProgramUnit sourceName or whatever is in the AST.
puSrcName :: ProgramUnit (Analysis a) -> ProgramUnitName
puSrcName pu
  | Just n <- sourceName (getAnnotation pu) = Named n
  | otherwise                               = getName pu

-- | Create analysis annotations for the program, saving the original
-- annotations.
initAnalysis :: Functor b => b a -> b (Analysis a)
initAnalysis = fmap analysis0

-- | Remove analysis annotations from the program, restoring the
-- original annotations.
stripAnalysis :: Functor b => b (Analysis a) -> b a
stripAnalysis = fmap prevAnnotation

--------------------------------------------------

-- | Return list of expressions used as the left-hand-side of
-- assignment statements (including for-loops and function-calls by reference).
lhsExprs :: forall a b . (Data a, Data (b a)) => b a -> [Expression a]
lhsExprs x = concatMap lhsOfStmt (universeBi x)
  where
    lhsOfStmt :: Statement a -> [Expression a]
    lhsOfStmt (StExpressionAssign _ _ e e') = e : onExprs e'
    lhsOfStmt (StCall _ _ _ (Just aexps)) = filter isLExpr argExps ++ concatMap onExprs argExps
       where argExps = map extractExp . aStrip $ aexps
    lhsOfStmt s =  onExprs s

    onExprs :: (Data a, Data (c a)) => c a -> [Expression a]
    onExprs = concatMap lhsOfExp . universeBi
    lhsOfExp :: Expression a -> [Expression a]
    lhsOfExp (ExpFunctionCall _ _ _ (Just aexps)) = fstLvl aexps
    lhsOfExp _ = []

    fstLvl = filter isLExpr . map extractExp . aStrip
    extractExp (Argument _ _ _ exp) = exp

-- | Return list of expressions that are not "left-hand-side" of
-- assignment statements.
rhsExprs :: (Data a, Data (b a)) => b a -> [Expression a]
rhsExprs x = concat [ blockRhsExprs b | b <- universeBi x ]

-- | Is this an expression capable of assignment?
isLExpr :: Expression a -> Bool
isLExpr (ExpValue _ _ (ValVariable {}))  = True
isLExpr (ExpSubscript _ _ _ _)           = True
isLExpr _                                = False

-- | Set of names found in an AST node.
allVars :: forall a b. (Data a, Data (b (Analysis a))) => b (Analysis a) -> [Name]
allVars b = [ varName v | v@(ExpValue _ _ (ValVariable _)) <- uniBi b ]
  where
    uniBi x = universeBi x :: [Expression (Analysis a)]

-- | Initiate (lazy) computation of all LHS variables for each node of
-- the AST so that it may be accessed later.
analyseAllLhsVars :: forall a . Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseAllLhsVars = (transformBi :: TransFunc Block ProgramFile a) analyseAllLhsVars1 .
                    (transformBi :: TransFunc Statement ProgramFile a) analyseAllLhsVars1 .
                    (transformBi :: TransFunc DoSpecification ProgramFile a) analyseAllLhsVars1

analyseAllLhsVars1 :: (Annotated f, Data (f (Analysis a)), Data a) => f (Analysis a) -> f (Analysis a)
analyseAllLhsVars1 x = modifyAnnotation (\ a -> a { allLhsVarsAnn = computeAllLhsVars x }) x

-- | Set of names found in the parts of an AST that are the target of
-- an assignment statement.
-- allLhsVars :: (Annotated b, Data a, Data (b (Analysis a))) => b (Analysis a) -> [Name]
allLhsVars :: Data a => Block (Analysis a) -> [Name]
allLhsVars x = allLhsVarsAnn . getAnnotation $ x

allLhsVarsDoSpec :: Data a => DoSpecification (Analysis a) -> [Name]
allLhsVarsDoSpec x = computeAllLhsVars x

-- | Set of names found in the parts of an AST that are the target of
-- an assignment statement.
computeAllLhsVars :: forall a b . (Data a, Data (b (Analysis a))) => b (Analysis a) -> [Name]
computeAllLhsVars = concatMap lhsOfStmt . universeBi
  where
    lhsOfStmt :: Statement (Analysis a) -> [Name]
    lhsOfStmt (StExpressionAssign _ _ e e') = match' e : onExprs e'
    lhsOfStmt (StCall _ _ f@(ExpValue _ _ (ValIntrinsic _)) _)
      | Just defs <- intrinsicDefs f = defs
    lhsOfStmt (StCall _ _ _ (Just aexps)) = concatMap (match'' . extractExp) (aStrip aexps)
    lhsOfStmt s = onExprs s

    onExprs :: (Data (c (Analysis a))) => c (Analysis a) -> [Name]
    onExprs = concatMap lhsOfExp . universeBi

    lhsOfExp :: Expression (Analysis a) -> [Name]
    lhsOfExp (ExpFunctionCall _ _ _ (Just aexps)) = concatMap (match . extractExp) (aStrip aexps)
    lhsOfExp _ = []

    extractExp (Argument _ _ _ exp) = exp

    -- Match and give the varname for LHS of statement
    match' v@(ExpValue _ _ (ValVariable {})) = varName v
    match' (ExpSubscript _ _ v@(ExpValue _ _ (ValVariable {})) _) = varName v
    match' (ExpDataRef _ _ v _) = match' v
    match' e = error $ "An unexpected LHS to an expression assign: " ++ show (fmap (const ()) e)

    -- Match and give the varname of LHSes which occur in subroutine calls
    match'' v@(ExpValue _ _ (ValVariable {})) = [varName v]
    match'' (ExpSubscript _ _ v@(ExpValue _ _ (ValVariable {})) _) = [varName v]
    match'' (ExpDataRef _ _ v _) = match'' v
    match'' e = onExprs e

   -- Match and give the varname of LHSes which occur in function calls
    match v@(ExpValue _ _ (ValVariable {})) = [varName v]
    match (ExpSubscript _ _ v@(ExpValue _ _ (ValVariable {})) _) = [varName v]
    match (ExpDataRef _ _ e _) = match e
    match e = []

-- | Set of expressions used -- not defined -- by an AST-block.
blockRhsExprs :: Data a => Block a -> [Expression a]
blockRhsExprs (BlStatement _ _ _ s) = statementRhsExprs s
blockRhsExprs (BlDo _ _ _ _ _ (Just (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2)) _ _)
  | ExpSubscript _ _ _ subs <- lhs = universeBi (rhs, e1, e2) ++ universeBi subs
  | otherwise                      = universeBi (rhs, e1, e2)
blockRhsExprs (BlDoWhile _ _ e1 _ _ e2 _ _) = universeBi (e1, e2)
blockRhsExprs (BlIf _ _ e1 _ e2 _ _)        = universeBi (e1, e2)
blockRhsExprs b                             = universeBi b

-- | Set of expression used -- not defined -- by a statement
statementRhsExprs :: Data a => Statement a -> [Expression a]
statementRhsExprs (StExpressionAssign _ _ lhs rhs)
 | ExpSubscript _ _ _ subs <- lhs = universeBi rhs ++ universeBi subs
 | otherwise                      = universeBi rhs
statementRhsExprs (StDeclaration {}) = []
statementRhsExprs (StIfLogical _ _ _ s) = statementRhsExprs s
statementRhsExprs (StDo _ _ _ l s) = (universeBi l) ++ doSpecRhsExprs s
  where doSpecRhsExprs (Just (DoSpecification _ _ s e1 e2)) =
           (e1 : (universeBi e2)) ++ statementRhsExprs s
        doSpecRhsExprs Nothing = []
statementRhsExprs s = universeBi s

-- | Set of names used -- not defined -- by an AST-block.
blockVarUses :: Data a => Block (Analysis a) -> [Name]
blockVarUses (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ concatMap allVars (aStrip subs)
  | otherwise                      = allVars rhs
blockVarUses (BlDo _ _ _ _ _ (Just (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2)) _ _)
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ allVars e1 ++ maybe [] allVars e2 ++ concatMap allVars (aStrip subs)
  | otherwise                      = allVars rhs ++ allVars e1 ++ maybe [] allVars e2
blockVarUses (BlStatement _ _ _ (StDeclaration {})) = []
blockVarUses (BlStatement _ _ _ (StCall _ _ f@(ExpValue _ _ (ValIntrinsic _)) _))
  | Just uses <- intrinsicUses f = uses
blockVarUses (BlStatement _ _ _ (StCall _ _ _ (Just aexps))) = allVars aexps
blockVarUses (BlDoWhile _ _ e1 _ _ e2 _ _) = maybe [] allVars e1 ++ allVars e2
blockVarUses (BlIf _ _ e1 _ e2 _ _)        = maybe [] allVars e1 ++ concatMap (maybe [] allVars) e2
blockVarUses b                             = allVars b

-- | Set of names defined by an AST-block.
blockVarDefs :: Data a => Block (Analysis a) -> [Name]
blockVarDefs b@(BlStatement _ _ _ st) = allLhsVars b
blockVarDefs (BlDo _ _ _ _ _ (Just doSpec) _ _)  = allLhsVarsDoSpec doSpec
blockVarDefs _                      = []

-- form name: n[i]
dummyArg :: Name -> Int -> Name
dummyArg n i = n ++ "[" ++ show i ++ "]"

-- return dummy arg names defined by intrinsic
intrinsicDefs :: Expression (Analysis a) -> Maybe [Name]
intrinsicDefs = fmap fst . intrinsicDefsUses

-- return dummy arg names used by intrinsic
intrinsicUses :: Expression (Analysis a) -> Maybe [Name]
intrinsicUses = fmap snd . intrinsicDefsUses

-- return dummy arg names (defined, used) by intrinsic
intrinsicDefsUses :: Expression (Analysis a) -> Maybe ([Name], [Name])
intrinsicDefsUses f = both (map (dummyArg (varName f))) <$> getIntrinsicDefsUses (srcName f) allIntrinsics
  where both f (x, y) = (f x, f y)

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

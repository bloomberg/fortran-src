{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Fortran.Util.JSON where

import Prelude hiding (Ordering(..))
import Data.Aeson.Types (ToJSON(..), object, (.=), Pair, Options(..), defaultOptions
                        ,camelTo2, genericToJSON, SumEncoding(..))
import qualified Data.Aeson.Types as J
import Data.Text (Text)

import Language.Fortran.AST
import Language.Fortran.Util.Position


jsonOptions :: Options
jsonOptions = defaultOptions
  { omitNothingFields = True
  , sumEncoding = ObjectWithSingleField
  }

tag :: Text -> [Pair] -> J.Value
tag t kvs = object $ ["tag" .= t] ++ kvs

-- instance ToJSON Position where
--   toJSON (Position _ col line) = object
--     [ "column" .= col, "line" .= line ]

instance ToJSON SrcSpan where
  toJSON = toJSON . show
  -- toJSON (SrcSpan start end) = object
  --   [ "start" .= start, "end" .= end ]

instance ToJSON (t a) => ToJSON (AList t a) where
  toJSON (AList _ s xs) = toJSON xs

instance ToJSON BaseType where
  toJSON t = case t of
    TypeInteger -> toJSON @String "integer"
    TypeReal -> toJSON @String "real"
    TypeDoublePrecision -> toJSON @String "double_precision"
    TypeComplex -> toJSON @String "complex"
    TypeDoubleComplex -> toJSON @String "double_complex"
    TypeLogical -> toJSON @String "logical"
    TypeCharacter -> toJSON @String "character"
    TypeByte -> toJSON @String "byte"
    TypeRecord r -> toJSON @String r

instance ToJSON a => ToJSON (TypeSpec a) where
  toJSON (TypeSpec _ s t mSel) = object
    [ "span" .= s, "base_type" .= t, "selector" .= mSel ]

instance ToJSON a => ToJSON (Selector a) where
  toJSON (Selector _ s mLen mKind) = object
    [ "span" .= s, "length" .= mLen, "kind" .= mKind ]

instance ToJSON MetaInfo where
  toJSON = genericToJSON jsonOptions{fieldLabelModifier = camelTo2 '_' . drop 2}

instance ToJSON a => ToJSON (ProgramFile a) where
  toJSON (ProgramFile info pus) = object
    [ "meta_info" .= info, "program" .= pus ]

instance ToJSON a => ToJSON (ProgramUnit a) where
  toJSON pu = case pu of
    PUMain _ s name blocks pus -> tag "main"
      ["span" .= s, "name" .= name, "blocks" .= blocks, "subprograms" .= pus]
    PUModule _ s name blocks pus -> tag "module"
      ["span" .= s, "name" .= name, "blocks" .= blocks, "subprograms" .= pus]
    PUSubroutine _ s r name args blocks pus -> tag "subroutine"
      [ "span" .= s, "recursive" .= r, "name" .= name
      , "arguments" .= args, "blocks" .= blocks
      , "subprograms" .= pus
      ]
    PUFunction _ s t r name args res blocks pus -> tag "function"
      [ "span" .= s, "recursive" .= r, "name" .= name
      , "type" .= t
      , "arguments" .= args, "blocks" .= blocks
      , "result" .= res
      , "subprograms" .= pus
      ]
    PUBlockData _ s name blocks -> tag "block_data"
      ["span" .= s, "name" .= name, "blocks" .= blocks]
    PUComment _ s (Comment c) -> tag "comment"
      ["span" .= s, "contents" .= c]

instance ToJSON a => ToJSON (Block a) where
  toJSON bl = case bl of
    BlStatement _ s l st -> tag "statement"
      ["span" .= s, "label" .= l, "statement" .= st]
    BlIf _ s l _ conds blocks endlabel -> tag "if"
      [ "span" .= s, "label" .= l, "conditions" .= conds
      , "blocks" .= blocks, "end_label" .= endlabel ]
    BlCase _ s l _ scrut ranges blocks endlabel -> tag "select"
      [ "span" .= s, "label" .= l, "scrutinee" .= scrut
      , "ranges" .= ranges, "blocks" .= blocks, "end_label" .= endlabel ]
    BlDo _ s l _ target dospec body endlabel  -> tag "do"
      [ "span" .= s, "label" .= l, "target" .= target
      , "do_spec" .= dospec, "body" .= body, "end_label" .= endlabel]
    BlDoWhile _ s l _ target cond body endlabel  -> tag "do_while"
      [ "span" .= s, "label" .= l, "target" .= target, "condition" .= cond
      , "body" .= body, "end_label" .= endlabel]
    BlInterface _ s l decls blocks  -> tag "interface"
      [ "span" .= s, "label" .= l
      , "declarations" .= decls, "blocks" .= blocks]

instance ToJSON a => ToJSON (Statement a) where
  toJSON st = case st of
    StDeclaration _ s t attrs decls -> tag "declaration"
      ["span" .= s, "type" .= t, "attributes" .= attrs, "declarators" .= decls]
    StStructure _ s name decls -> tag "structure"
      ["span" .= s, "name" .= name, "fields" .= decls]
    StIntent {} -> error "unexpected StIntent"
    StOptional {} -> error "unexpected StOptional"
    StPublic {} -> error "unexpected StPublic"
    StPrivate {} -> error "unexpected StPrivate"
    StSave _ s args -> tag "save"
      ["span" .= s, "variables" .= args]
    StDimension _ s decls -> tag "dimension"
      ["span" .= s, "declarators" .= decls]
    StAllocatable {} -> error "unexpected StAllocatable"
    StPointer _ s decls -> tag "pointer"
      ["span" .= s, "declarators" .= decls]
    StTarget {} -> error "unexpected StTarget"
    StData _ s args -> tag "data"
      ["span" .= s, "data_groups" .= args]
    StAutomatic _ s args -> tag "automatic"
      ["span" .= s, "declarators" .= args]
    StNamelist {} -> error "unexpected StNamelist"
    StParameter _ s args -> tag "parameter"
      ["span" .= s, "declarators" .= args]
    StExternal _ s args -> tag "external"
      ["span" .= s, "arguments" .= args]
    StIntrinsic _ s args -> tag "intrinsic"
      ["span" .= s, "arguments" .= args]
    StCommon _ s args -> tag "common"
      ["span" .= s, "common_groups" .= args]
    StEquivalence _ s args -> tag "equivalence"
      ["span" .= s, "groups" .= args]
    StFormat{} -> error "unexpected StFormat"
    -- StFormat _ s fmts -> tag "format"
    --   ["span" .= s, "items" .= fmts]
    StImplicit _ s itms -> tag "implicit"
      ["span" .= s, "items" .= itms]
    StEntry _ s v args _ -> tag "entry"
      ["span" .= s, "name" .= v, "arguments" .= args]
    StInclude _ s path blocks -> tag "include"
      ["span" .= s, "path" .= path, "blocks" .= blocks]
    StDo _ s _ lbl spec -> tag "do"
      ["span" .= s, "label" .= lbl, "do_spec" .= spec]
    StDoWhile _ s _ lbl cond -> tag "do_while"
      ["span" .= s, "label" .= lbl, "condition" .= cond]
    StEnddo _ s _ -> tag "end_do" ["span" .= s]
    StCycle _ s _ -> tag "cycle" ["span" .= s]
    StExit _ s _ -> tag "exit" ["span" .= s]
    StIfLogical _ s cond stmt -> tag "if_logical"
      ["span" .= s, "condition" .= cond, "statement" .= stmt]
    StIfArithmetic _ s exp lt eq gt -> tag "if_arithmetic"
      ["span" .= s, "expression" .= exp, "less" .= lt, "equal" .= eq, "greater" .= gt]
    StIfThen _ s _ cond -> tag "if_then"
      ["span" .= s, "condition" .= cond]
    StElse _ s _ -> tag "else" ["span" .= s]
    StElsif _ s _ cond -> tag "else_if"
      ["span" .= s, "condition" .= cond]
    StEndif _ s _ -> tag "end_if" ["span" .= s]
    StSelectCase _ s _ exp -> tag "select_case"
      ["span" .= s, "expression" .= exp]
    StCase _ s _ idxs -> tag "case"
      ["span" .= s, "indices" .= idxs]
    StEndcase _ s _ -> tag "end_select" ["span" .= s]
    StFunction _ s fn args body -> tag "function"
      ["span" .= s, "name" .= fn, "arguments" .= args, "body" .= body]
    StExpressionAssign _ s tgt exp -> tag "assign_expression"
      ["span" .= s, "target" .= tgt, "expression" .= exp]
    StPointerAssign{} -> error "unexpected StPointerAssign"
    StLabelAssign _ s lbl tgt -> tag "assign_label"
      ["span" .= s, "target" .= tgt, "label" .= lbl]
    StGotoUnconditional _ s tgt -> tag "goto"
      ["span" .= s, "target" .= tgt]
    StGotoAssigned _ s tgt lbls -> tag "goto_assigned"
      ["span" .= s, "target" .= tgt, "labels" .= lbls]
    StGotoComputed _ s lbls tgt -> tag "goto_computed"
      ["span" .= s, "target" .= tgt, "labels" .= lbls]
    StCall _ s fn args -> tag "call"
      ["span" .= s, "function" .= fn, "arguments" .= args]
    StReturn _ s tgt -> tag "return"
      ["span" .= s, "target" .= tgt]
    StContinue _ s -> tag "continue" ["span" .= s]
    StStop _ s msg -> tag "stop"
      ["span" .= s, "message" .= msg]
    StPause _ s msg -> tag "pause"
      ["span" .= s, "message" .= msg]
    StRead _ s fmt args -> tag "read"
      ["span" .= s, "format" .= fmt, "arguments" .= args]
    StRead2 _ s fmt args -> tag "read"
      ["span" .= s, "format" .= fmt, "arguments" .= args]
    StWrite _ s fmt args -> tag "write"
      ["span" .= s, "format" .= fmt, "arguments" .= args]
    StPrint _ s fmt args -> tag "print"
      ["span" .= s, "format" .= fmt, "arguments" .= args]
    StTypeBI _ s fmt args -> tag "type"
      ["span" .= s, "format" .= fmt, "arguments" .= args]
    StOpen _ s spec -> tag "open"
      ["span" .= s, "specification" .= spec]
    StClose _ s spec -> tag "close"
      ["span" .= s, "specification" .= spec]
    StInquire _ s spec -> tag "inquire"
      ["span" .= s, "specification" .= spec]
    StRewind _ s spec -> tag "rewind"
      ["span" .= s, "specification" .= spec]
    StRewind2 _ s spec -> tag "rewind"
      ["span" .= s, "specification" .= spec]
    StBackspace _ s spec -> tag "backspace"
      ["span" .= s, "specification" .= spec]
    StBackspace2 _ s spec -> tag "backspace"
      ["span" .= s, "specification" .= spec]
    StEndfile _ s spec -> tag "endfile"
      ["span" .= s, "specification" .= spec]
    StEndfile2 _ s spec -> tag "endfile"
      ["span" .= s, "specification" .= spec]
    StAllocate{} -> error "unexpected StAllocate"
    StNullify{} -> error "unexpected StNullify"
    StDeallocate{} -> error "unexpected StDeallocate"
    StWhere{} -> error "unexpected StWhere"
    StWhereConstruct{} -> error "unexpected StWhereConstruct"
    StElsewhere{} -> error "unexpected StElsewhere"
    StEndWhere{} -> error "unexpected StEndwhere"
    StUse{} -> error "unexpected StUse"
    StModuleProcedure{} -> error "unexpected StModuleProcedure"
    StType{} -> error "unexpected StType"
    StEndType{} -> error "unexpected StEndtype"
    StSequence{} -> error "unexpected StSequence"
    StForall{} -> error "unexpected StForall"
    StFormatBogus _ s fmt -> tag "format"
      ["span" .= s, "contents" .= fmt]

instance ToJSON a => ToJSON (Argument a) where
  toJSON (Argument _ s name exp) = tag "argument"
    ["span" .= s, "name" .= name, "expression" .= exp]

instance ToJSON a => ToJSON (Attribute a) where
  toJSON attr = case attr of
    AttrParameter _ s -> tag "parameter" ["span" .= s]
    AttrPublic{} -> error "unexpected AttrPublic"
    AttrPrivate{} -> error "unexpected AttrPrivate"
    AttrAllocatable{} -> error "unexpected AttrAllocatable"
    AttrDimension _ s dims -> tag "dimension" ["span" .= s, "dimensions" .= dims]
    AttrExternal _ s -> tag "external" ["span" .= s]
    AttrIntent{} -> error "unexpected AttrIntent"
    AttrOptional{} -> error "unexpected AttrOptional"
    AttrPointer _ s -> tag "pointer" ["span" .= s]
    AttrSave _ s -> tag "save" ["span" .= s]
    AttrTarget{} -> error "unexpected AttrTarget"

instance ToJSON a => ToJSON (ControlPair a) where
  toJSON (ControlPair _ s name exp) = tag "control_pair"
    ["span" .= s, "name" .= name, "expression" .= exp]

instance ToJSON a => ToJSON (ImpList a) where
  toJSON (ImpList _ s t itms) = tag "implicit_spec"
    ["span" .= s, "type" .= t, "items" .= itms]

instance ToJSON a => ToJSON (ImpElement a) where
  toJSON imp = case imp of
    ImpCharacter _ s c -> tag "implicit_char"
      ["span" .= s, "char" .= c]
    ImpRange _ s c1 c2  -> tag "implicit_range"
      ["span" .= s, "lower" .= c1, "upper" .= c2]

instance ToJSON a => ToJSON (CommonGroup a) where
  toJSON (CommonGroup _ s name exps) = tag "common_group"
    ["span" .= s, "name" .= name, "expressions" .= exps]

instance ToJSON a => ToJSON (DataGroup a) where
  toJSON (DataGroup _ s names exps) = tag "data_group"
    ["span" .= s, "names" .= names, "initializers" .= exps]

instance ToJSON a => ToJSON (StructureItem a) where
  toJSON si = case si of
    StructFields _ s t attrs decls -> tag "fields"
      ["span" .= s, "type" .= t, "attributes" .= attrs, "declarators" .= decls]
    StructUnion _ s maps -> tag "union"
      ["span" .= s, "maps" .= maps]

instance ToJSON a => ToJSON (UnionMap a) where
  toJSON (UnionMap _ s flds) = tag "union_map"
    ["span" .= s, "fields" .= flds]

instance ToJSON a => ToJSON (DoSpecification a) where
  toJSON (DoSpecification _ s init lim incr) = tag "do_spec"
    ["span" .= s, "initial" .= init, "limit" .= lim, "increment" .= incr]

instance ToJSON a => ToJSON (Expression a) where
  toJSON exp = case exp of
    ExpValue _ s val -> tag "value"
      ["span" .= s, "value" .= val]
    ExpBinary _ s b e1 e2 -> tag "binary_op"
      ["span" .= s, "binary_op" .= b, "left" .= e1, "right" .= e2]
    ExpUnary _ s u e -> tag "unary_op"
      ["span" .= s, "unary_op" .= u, "expression" .= e]
    ExpSubscript _ s e idxs -> tag "subscript"
      ["span" .= s, "expression" .= e, "indices" .= idxs]
    ExpDataRef _ s e1 e2 -> tag "deref"
      ["span" .= s, "expression" .= e1, "field" .= e2]
    ExpFunctionCall _ s fn args -> tag "function_call"
      ["span" .= s, "function" .= fn, "arguments" .= args]
    ExpImpliedDo _ s exps spec -> tag "implied_do"
      ["span" .= s, "expressions" .= exps, "do_spec" .= spec]
    ExpInitialisation _ s exps -> tag "initialisation"
      ["span" .= s, "expressions" .= exps]
    ExpReturnSpec _ s tgt -> tag "return_spec"
      ["span" .= s, "target" .= tgt]
    ExpByValue _ s exp -> tag "%val"
      ["span" .= s, "expression" .= exp]

instance ToJSON a => ToJSON (Index a) where
  toJSON idx = case idx of
    IxSingle _ s _ e -> tag "index_single"
      ["span" .= s, "index" .= e]
    IxRange _ s l u st -> tag "index_range"
      ["span" .= s, "lower" .= l, "upper" .= u, "stride" .= st]

instance ToJSON a => ToJSON (Value a) where
  toJSON v = case v of
    ValInteger i -> tag "integer" ["value" .= i]
    ValReal i -> tag "real" ["value" .= i]
    ValComplex r i -> tag "complex" ["real" .= r, "imaginary" .= i]
    ValString i -> tag "string" ["value" .= i]
    ValHollerith i -> tag "hollerith" ["value" .= i]
    ValVariable i -> tag "variable" ["value" .= i]
    ValIntrinsic i -> tag "intrinsic" ["value" .= i]
    ValLogical i -> tag "logical" ["value" .= i]
    ValOperator{} -> error "unexpected ValOperator"
    ValAssignment{} -> error "unexpected ValAssignment"
    ValType{} -> error "unexpected ValType"
    ValStar{} -> tag "star" []

instance ToJSON a => ToJSON (Declarator a) where
  toJSON decl = case decl of
    DeclVariable _ s v len init -> tag "decl_variable"
      ["span" .= s, "variable" .= v, "length" .= len, "initial" .= init]
    DeclArray _ s arr dims len init -> tag "decl_array"
      ["span" .= s, "array" .= arr, "dimensions" .= dims, "length" .= len, "initial" .= init]

instance ToJSON a => ToJSON (DimensionDeclarator a) where
  toJSON (DimensionDeclarator _ s l u) = tag "dimension"
    ["span" .= s, "lower" .= l, "upper" .= u]

instance ToJSON UnaryOp where
  toJSON u = case u of
    Plus -> tag "plus" []
    Minus -> tag "minus" []
    Not -> tag "not" []
    UnCustom{} -> error "unexpected UnCustom"

instance ToJSON BinaryOp where
  toJSON b = case b of
    Addition -> tag "+" []
    Subtraction -> tag "-" []
    Multiplication -> tag "*" []
    Division -> tag "/" []
    Exponentiation -> tag "**" []
    Concatenation -> tag "//" []
    GT -> tag ">" []
    GTE -> tag ">=" []
    LT -> tag "<" []
    LTE -> tag "<=" []
    EQ -> tag "==" []
    NE -> tag "!=" []
    Or -> tag "or" []
    XOr -> tag "xor" []
    And -> tag "and" []
    Equivalent -> tag "eqv" []
    NotEquivalent -> tag "neqv" []
    BinCustom{} -> error "unexpected BinCustom"

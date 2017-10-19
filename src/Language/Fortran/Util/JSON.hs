{-# LANGUAGE OverloadedStrings #-}
module Language.Fortran.Util.JSON where

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

instance ToJSON Position where
  toJSON (Position _ col line) = object
    [ "column" .= col, "line" .= line ]

instance ToJSON SrcSpan where
  toJSON (SrcSpan start end) = object
    [ "start" .= start, "end" .= end ]

instance ToJSON (t a) => ToJSON (AList t a) where
  toJSON (AList _ s xs) = toJSON xs

instance ToJSON BaseType where
  toJSON = genericToJSON jsonOptions{constructorTagModifier = camelTo2 '_' . drop 4}

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
    BlIf _ s l name conds blocks endlabel -> tag "if"
      [ "span" .= s, "label" .= l, "conditions" .= conds
      , "blocks" .= blocks, "end_label" .= endlabel ]
    BlCase _ s l _ scrut ranges blocks endlabel -> tag "select"
      [ "span" .= s, "label" .= l, "scrutinee" .= scrut
      , "ranges" .= ranges, "blocks" .= blocks, "end_label" .= endlabel ]
    BlDo _ s l name target dospec body endlabel  -> tag "do"
      [ "span" .= s, "label" .= l, "target" .= target
      , "do_spec" .= dospec, "body" .= body, "end_label" .= endlabel]
    BlDoWhile _ s l name cond dospec body endlabel  -> tag "do_while"
      [ "span" .= s, "label" .= l, "condition" .= cond
      , "body" .= body, "end_label" .= endlabel]
    BlInterface _ s l decls blocks  -> tag "interface"
      [ "span" .= s, "label" .= l
      , "declarations" .= decls, "blocks" .= blocks]

instance ToJSON a => ToJSON (Statement a) where
  toJSON st = case st of
    StDeclaration _ s t attrs decls -> tag "declaration"
      ["span" .= s, "type" .= t, "attributes" .= attrs, "declarators" .= decls]
    StIntent {} -> error "unexpected StIntent"
    StOptional {} -> error "unexpected StOptional"
    StPublic {} -> error "unexpected StPublic"
    StPrivate {} -> error "unexpected StPrivate"
    StSave _ s args -> tag "save"
      ["span" .= s, "variables" .= args]
    StDimension _ s decls -> tag "dimension"
      ["span" .= s, "declarators" .= decls]
    StAllocatable {} -> error "unexpected StAllocatable"
    StPointer {} -> error "unexpected StPointer"
    StTarget {} -> error "unexpected StTarget"
    StData _ s args -> tag "data"
      ["span" .= s, "data_groups" .= args]
    StAutomatic _ s args -> tag "automatic"
      ["span" .= s, "declarators" .= args]
    StNamelist {} -> error "unexpected StNamelist"
    StParameter _ s args -> tag "parameter"
      ["span" .= s, "declarators" .= args]
    StExternal _ s args -> tag "external"
      ["span" .= s, "declarators" .= args]
    StIntrinsic {} -> error "unexpected StIntrinsic"
    StCommon _ s args -> tag "common"
      ["span" .= s, "groups" .= args]
    StEquivalence _ s args -> tag "equivalence"
      ["span" .= s, "groups" .= args]
    StFormat _ s fmts -> tag "format"
      ["span" .= s, "items" .= fmts]
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
    StFunction{} -> error "unexpected StFunction"
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

instance ToJSON Intent where

instance ToJSON a => ToJSON (ControlPair a) where

instance ToJSON a => ToJSON (ImpList a) where

instance ToJSON a => ToJSON (ImpElement a) where

instance ToJSON a => ToJSON (CommonGroup a) where

instance ToJSON a => ToJSON (DataGroup a) where

instance ToJSON a => ToJSON (UnionMap a) where

instance ToJSON a => ToJSON (FormatItem a) where

instance ToJSON a => ToJSON (DoSpecification a) where

instance ToJSON a => ToJSON (Expression a) where

instance ToJSON a => ToJSON (Index a) where

instance ToJSON a => ToJSON (Value a) where

instance ToJSON a => ToJSON (Declarator a) where

instance ToJSON a => ToJSON (DimensionDeclarator a) where

instance ToJSON UnaryOp where

instance ToJSON BinaryOp where

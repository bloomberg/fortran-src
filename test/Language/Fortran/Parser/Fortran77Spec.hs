module Language.Fortran.Parser.Fortran77Spec where

import Test.Hspec
import TestUtil

import Language.Fortran.Parser.Fortran77
import Language.Fortran.Lexer.FixedForm (initParseState)
import Language.Fortran.ParserMonad (FortranVersion(..), evalParse, fromParseResultUnsafe)
import Language.Fortran.AST
import qualified Data.ByteString.Char8 as B

eParser :: String -> Expression ()
eParser sourceCode =
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
  where
    paddedSourceCode = B.pack $ "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran77 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode =
  evalParse statementParser $ initParseState (B.pack sourceCode) Fortran77 "<unknown>"

slParser :: String -> Statement ()
slParser sourceCode =
  evalParse statementParser $ initParseState (B.pack sourceCode) Fortran77Legacy "<unknown>"

iParser :: String -> [Block ()]
iParser sourceCode =
  fromParseResultUnsafe $ includeParser Fortran77Legacy (B.pack sourceCode) "<unknown>"

pParser :: String -> ProgramFile ()
pParser source = fromParseResultUnsafe $ fortran77Parser (B.pack source) "<unknown>"

spec :: Spec
spec =
  describe "Fortran 77 Parser" $ do
    describe "IO" $ do
      it "parses 'print *, 9000" $ do
        let expectedSt = StPrint () u starVal $ Just (AList () u [ intGen 9000 ])
        sParser "      print *, 9000" `shouldBe'` expectedSt

      it "parses 'write (UNIT=6, FORMAT=*)" $ do
        let cp1 = ControlPair () u (Just "unit") (intGen 6)
        let cp2 = ControlPair () u (Just "format") starVal
        let expectedSt = StWrite () u (AList () u [cp1, cp2]) Nothing
        sParser "      write (UNIT=6, FORMAT=*)" `shouldBe'` expectedSt

      it "parses 'endfile i" $
        sParser "      endfile i" `shouldBe'` StEndfile2 () u (varGen "i")

      it "parses 'read *, (x, y(i), i = 1, 10, 2)'" $ do
        let stAssign = StExpressionAssign () u (varGen "i") (intGen 1)
        let doSpec = DoSpecification () u stAssign (intGen 10) (Just $ intGen 2)
        let impliedDoVars = AList () u [ varGen "x", ExpSubscript () u (varGen "y") (AList () u [ IxSingle () u Nothing $ varGen "i" ])]
        let impliedDo = ExpImpliedDo () u impliedDoVars doSpec
        let iolist = AList () u [ impliedDo ]
        let expectedSt = StRead2 () u starVal (Just iolist)
        sParser "      read *, (x, y(i), i = 1, 10, 2)" `shouldBe'` expectedSt

    it "parses '(x, y(i), i = 1, 10, 2)'" $ do
      let stAssign = StExpressionAssign () u (varGen "i") (intGen 1)
      let doSpec = DoSpecification () u stAssign (intGen 10) (Just $ intGen 2)
      let impliedDoVars = AList () u [ varGen "x", ExpSubscript () u (varGen "y") (AList () u [ IxSingle () u Nothing $ varGen "i" ])]
      let impliedDo = ExpImpliedDo () u impliedDoVars doSpec
      eParser "(x, y(i), i = 1, 10, 2)" `shouldBe'` impliedDo

    it "parses main program unit" $ do
      let decl = DeclVariable () u (varGen "x") Nothing Nothing
      let st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
      let bl = BlStatement () u Nothing st
      let pu = ProgramFile mi77 [ PUMain () u (Just "hello") [ bl ] Nothing ]
      pParser exampleProgram1 `shouldBe'` pu

    it "parses block data unit" $ do
      let decl = DeclVariable () u (varGen "x") Nothing Nothing
      let st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
      let bl = BlStatement () u Nothing st
      let pu = ProgramFile mi77 [ PUBlockData () u (Just "hello") [ bl ] ]
      pParser exampleProgram2 `shouldBe'` pu

    it "parses 'intrinsic cosh, sin'" $ do
      let fun1 = ExpValue () u (ValVariable "cosh")
      let fun2 = ExpValue () u (ValVariable "sin")
      let st = StIntrinsic () u (AList () u [ fun1, fun2 ])
      sParser "      intrinsic cosh, sin" `shouldBe'` st

    it "parses 'intrinsic real" $ do
      let fun = ExpValue () u (ValVariable "real")
      let st = StIntrinsic () u (AList () u [ fun ])
      sParser "      intrinsic real" `shouldBe'` st

    describe "CHARACTER" $ do
      it "parses character literal assignment" $ do
        let rhs = ExpValue () u (ValString "hello 'baby")
        let st = StExpressionAssign () u (varGen "xyz") rhs
        sParser "      xyz = 'hello ''baby'" `shouldBe'` st

      it "string concatenation" $ do
        let str1 = ExpValue () u (ValString "hello ")
        let str2 = ExpValue () u (ValString "world")
        let exp = ExpBinary () u Concatenation str1 str2
        eParser "'hello ' // 'world'" `shouldBe'` exp

    describe "Subscript like" $ do
      it "parses vanilla subscript" $ do
        let exp = ExpSubscript () u (varGen "a") (AList () u [ IxSingle () u Nothing $ varGen "x", IxSingle () u Nothing $ intGen 2, IxSingle () u Nothing $ intGen 3 ])
        eParser "a(x, 2, 3)" `shouldBe'` exp

      it "parses array declarator" $ do
        let dimDecls = [ DimensionDeclarator () u (Just $ intGen 1) (Just $ intGen 2)
                       , DimensionDeclarator () u Nothing (Just $ intGen 15)
                       , DimensionDeclarator () u (Just $ varGen "x") (Just $ starVal) ]
        let decl = DeclArray () u (varGen "a") (AList () u dimDecls) Nothing Nothing
        let st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
        sParser "      integer a(1:2, 15, x:*)" `shouldBe'` st

      it "parses character substring" $ do
        let indicies = [ ixSinGen 1, ixSinGen 2, ixSinGen 3 ]
        let subExp = ExpSubscript () u (varGen "a")  (AList () u indicies)
        let range = IxRange () u Nothing (Just $ intGen 10) Nothing
        let exp = ExpSubscript () u subExp (AList () u [ range ])
        eParser "a(1, 2, 3)(:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let exp = ExpSubscript () u (varGen "a") (AList () u [ ixRanGen 5 10 ])
        eParser "a(5:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let range = IxRange () u (Just $ intGen 5) Nothing Nothing
        let exp = ExpSubscript () u (varGen "a") (AList () u [ range ])
        eParser "a(5:)" `shouldBe'` exp

    describe "GOTO" $ do
      it "parses computed GOTO with integer expression" $ do
        let exp = ExpBinary () u Multiplication (intGen 42) (intGen 24)
        let st = StGotoComputed () u (AList () u [labelGen 10, labelGen 20, labelGen 30]) exp
        sParser "      GOTO (10, 20, 30), 42 * 24" `shouldBe'` st

      let gotoSt = StGotoAssigned () u (varGen "v") (Just (AList () u [labelGen 10, labelGen 20, labelGen 30]))
      it "parses assigned GOTO with comma" $
        sParser "      GOTO v, (10, 20, 30)" `shouldBe'` gotoSt

      it "parses assigned GOTO without comma" $
        sParser "      GOTO v (10, 20, 30)" `shouldBe'` gotoSt

    describe "IMPLICIT" $ do
      it "parses 'implicit none'" $ do
        let st = resetSrcSpan $ StImplicit () u Nothing
        sParser "      implicit none" `shouldBe'` st

      it "parses 'implicit character*30 (a, b, c), integer (a-z, l)" $ do
        let impEls = [ImpCharacter () u "a", ImpCharacter () u "b", ImpCharacter () u "c"]
        let selector = Selector () u (Just $ intGen 30) Nothing
        let imp1 = ImpList () u (TypeSpec () u TypeCharacter (Just selector)) $ AList () u impEls
        let imp2 = ImpList () u (TypeSpec () u TypeInteger Nothing) $ AList () u [ImpRange () u "a" "z", ImpCharacter () u "l"]
        let st = StImplicit () u $ Just $ AList () u [imp1, imp2]
        sParser "      implicit character*30 (a, b, c), integer (a-z, l)" `shouldBe'` st

    it "parses 'parameter (pi = 3.14, b = 'X' // 'O', d = k) '" $ do
      let sts = [ DeclVariable () u (varGen "pi") Nothing (Just $ realGen 3.14)
                , let e = ExpBinary () u Concatenation (strGen "X") (strGen "O")
                  in DeclVariable () u (varGen "b") Nothing (Just e)
                , DeclVariable () u (varGen "d") Nothing (Just $ varGen "k") ]
      let st = StParameter () u (AList () u sts)
      sParser "      parameter (pi = 3.14, b = 'X' // 'O', d = k)" `shouldBe'` st

    it "parses 'pause 'hello world''" $ do
      let st = StPause () u $ Just $ strGen "hello world"
      sParser "      pause 'hello world'" `shouldBe'` st

    describe "SAVE" $ do
      it "parses 'save /cb/, var, /key/'" $ do
        let saveArgs = [ varGen "cb", varGen "var", varGen "key" ]
        let st = StSave () u (Just $ AList () u saveArgs)
        sParser "      save /cb/, var, /key/" `shouldBe'` st

      it "parses 'save'" $
        sParser "      save" `shouldBe'` StSave () u Nothing

    it "parses '.true. .eqv. f(42) .neqv. x'" $ do
      let arg2 = ExpSubscript () u (varGen "f") $ AList () u [ ixSinGen 42 ]
      let arg3 = varGen "x"
      let subexp = ExpBinary () u Equivalent valTrue arg2
      let exp = ExpBinary () u NotEquivalent subexp arg3
      eParser ".true. .eqv. f(42) .neqv. x" `shouldBe'` exp

    it "parses 'entry me (a,b,*)'" $ do
      let func = ExpValue () u (ValVariable "me")
      let args = [ varGen "a", varGen "b", starVal ]
      let st = StEntry () u func (Just $ AList () u args) Nothing
      sParser "      entry me (a,b,*)" `shouldBe'` st

    it "parses 'character a*8'" $ do
      let decl = DeclVariable () u (varGen "a") (Just $ intGen 8) Nothing
      let typeSpec = TypeSpec () u TypeCharacter Nothing
      let st = StDeclaration () u typeSpec Nothing (AList () u [ decl ])
      sParser "      character a*8" `shouldBe'` st

    it "parses included files" $ do
      let decl = DeclVariable () u (varGen "a") Nothing Nothing
      let typeSpec = TypeSpec () u TypeInteger Nothing
      let st = StDeclaration () u typeSpec Nothing (AList () u [ decl ])
      let bl = BlStatement () u Nothing st
      iParser "      integer a" `shouldBe'` [bl]

    describe "Legacy Extensions" $ do
      it "parses structure/union/map blocks" $ do
        let src = init
                $ unlines [ "      structure /foo/"
                          , "        union"
                          , "          map"
                          , "            integer i"
                          , "          end map"
                          , "          map"
                          , "            real r"
                          , "          end map"
                          , "        end union"
                          , "      end structure"]
        let ds = [ UnionMap () u $ AList () u
                   [StructFields () u (TypeSpec () u TypeInteger Nothing) Nothing $
                    AList () u [DeclVariable () u (varGen "i") Nothing Nothing]]
                 , UnionMap () u $ AList () u
                   [StructFields () u (TypeSpec () u TypeReal Nothing) Nothing $
                    AList () u [DeclVariable () u (varGen "r") Nothing Nothing]]
                 ]
        let st = StStructure () u (Just "foo") $ AList () u [StructUnion () u $ AList () u ds]
        resetSrcSpan (slParser src) `shouldBe` st

      it "parses character declarations with unspecfied lengths" $ do
        let src = "      character s*(*)"
        let st = StDeclaration () u (TypeSpec () u TypeCharacter Nothing) Nothing $
                 AList () u [DeclVariable () u
                               (ExpValue () u (ValVariable "s"))
                               (Just (ExpValue () u ValStar))
                               Nothing]
        resetSrcSpan (slParser src) `shouldBe` st

      it "parses array initializers" $ do
        let src = "      integer xs(3) / 1, 2, 3 /"
        let inits = [ExpValue () u (ValInteger "1"), ExpValue () u (ValInteger "2"), ExpValue () u (ValInteger "3")]
        let st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing $
                 AList () u [DeclArray () u
                               (ExpValue () u (ValVariable "xs"))
                               (AList () u [DimensionDeclarator () u Nothing (Just (ExpValue () u (ValInteger "3")))])
                               Nothing
                               (Just (ExpInitialisation () u $ AList () u inits))]
        resetSrcSpan (slParser src) `shouldBe` st

        let src = "      character xs(2)*5 / 'hello', 'world' /"
        let inits = [ExpValue () u (ValString "hello"), ExpValue () u (ValString "world")]
        let st = StDeclaration () u (TypeSpec () u TypeCharacter Nothing) Nothing $
                 AList () u [DeclArray () u
                               (ExpValue () u (ValVariable "xs"))
                               (AList () u [DimensionDeclarator () u Nothing (Just (ExpValue () u (ValInteger "2")))])
                               (Just (ExpValue () u (ValInteger "5")))
                               (Just (ExpInitialisation () u $ AList () u inits))]
        resetSrcSpan (slParser src) `shouldBe` st

        let src = "      character xs*5(2) / 'hello', 'world' /"
        let inits = [ExpValue () u (ValString "hello"), ExpValue () u (ValString "world")]
        let st = StDeclaration () u (TypeSpec () u TypeCharacter Nothing) Nothing $
                 AList () u [DeclArray () u
                               (ExpValue () u (ValVariable "xs"))
                               (AList () u [DimensionDeclarator () u Nothing (Just (ExpValue () u (ValInteger "2")))])
                               (Just (ExpValue () u (ValInteger "5")))
                               (Just (ExpInitialisation () u $ AList () u inits))]
        resetSrcSpan (slParser src) `shouldBe` st

      it "parses subscripts in assignments" $ do
        let mkIdx i = IxSingle () u Nothing (ExpValue () u (ValInteger i))

        let src = "      x(0,1) = 0"
        let tgt = ExpSubscript () u (ExpValue () u (ValVariable "x")) (AList () u [mkIdx "0", mkIdx "1"])
        let st = StExpressionAssign () u tgt (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src) `shouldBe` st

        let src = "      x(0).foo = 0"
        let tgt = ExpDataRef () u (ExpSubscript () u (ExpValue () u (ValVariable "x")) (AList () u [mkIdx "0"])) (ExpValue () u (ValVariable "foo"))
        let st = StExpressionAssign () u tgt (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src) `shouldBe` st

        let src = "      x.foo = 0"
        let tgt = ExpDataRef () u (ExpValue () u (ValVariable "x")) (ExpValue () u (ValVariable "foo"))
        let st = StExpressionAssign () u tgt (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src) `shouldBe` st

        let src = "      x.foo(0) = 0"
        let tgt = ExpSubscript () u (ExpDataRef () u (ExpValue () u (ValVariable "x")) (ExpValue () u (ValVariable "foo"))) (AList () u [mkIdx "0"])
        let st = StExpressionAssign () u tgt (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src) `shouldBe` st

exampleProgram1 = unlines
  [ "      program hello"
  , "      integer x"
  , "      end" ]

exampleProgram2 = unlines
  [ "      block data hello"
  , "      integer x"
  , "      end" ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:

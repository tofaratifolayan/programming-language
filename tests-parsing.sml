(*  Parsing tests for COMP 323 WML.
*
*   N. Danner
*)

structure TestsParsing =
struct

  structure U = UnitTest
  structure TR = TestRunner
  structure TU = TestsUtility

  structure Ast = Ast
  structure Toks = Tokens

  structure T = TextIO

  (*  Lexing types and functions.
  *)
  type stream = MakeLex.stream

  (*  assertPgmParses pgmfile is a test that succeeds if the contents of pgmfile
  *  succesfully parse.
  *)
  fun assertPgmParses (pgmfile : string) : U.test =
  let
    val name = pgmfile

    fun test() : unit =
    let
      val strm = Lexer.streamifyInstream (TextIO.openIn pgmfile)
      val _ = Parse.parsePgm strm
    in
      ()
    end
  in
    U.doIt(name, test)
  end

  (*  assertPgmParsesCorrectly pgmfile exp is a test that succeeds if the
  *  contents of pgmfile parse to exp using Parse.parsePgm.
  *)
  fun assertPgmParsesCorrectly (pgmfile : string, exp : Ast.pgm) : U.test =
  let
    val name = pgmfile

    fun test() : Ast.pgm =
    let
      val strm = Lexer.streamifyInstream (TextIO.openIn pgmfile)
    in
      Parse.parsePgm strm
    end
  in
    U.makeTestEq(name, test, exp, Ast.pgmEq, Ast.pgmToString)
  end

  (*  assertPgmFailsParse pgmfile is a test that succeeds if parsing pgmfile
  *  with Parse.parsePgm raises NoParse or ExtraTokens.
  *)
  fun assertPgmFailsParse (pgmfile : string) : U.test =
  let
    val name = pgmfile

    val dummyStream = Lexer.streamifyInstream (TextIO.openString "")

    fun test() : unit =
    let
      val strm = Lexer.streamifyInstream (TextIO.openIn pgmfile)
      val _ = Parse.parsePgm strm
    in
      ()
    end
  in
    U.orTest(pgmfile, [
      U.assertExn("NoParse", test, Parse.NoParse ""),
      U.assertExn("ExtraTokens", test, Parse.ExtraTokens dummyStream)
    ])
  end


  (*  Parsing tests.
  *)

  (*  parsings = a list of values of the form (f, p) : string*Ast.pgm, where f
  *  is the name of a file in testfilesDir and p is the correct parse of f.
  *)
  local
    open Ast
  in
    val parsings = [
      ("ullman-ex2.01.wml",
        Program [ExprStm (Binop(IPlus, Int 1, Binop(ITimes, Int 2, Int 3)))]),
      ("ullman-ex2.02.wml",
        Program [ExprStm (Int(~170))]),
      ("ullman-ex2.03.wml",
        Program [
          ExprStm (Real ~123.0),
          ExprStm (Real 3E~3),
          ExprStm (Real 3.14e12)
        ]),
      ("ullman-ex2.04.wml",
        Program [ExprStm (Bool true), ExprStm (Bool false)]),
      ("ullman-ex2.05.wml",
        Program [ExprStm (Str "A\tB\tC\n1\t2\t3\n")]),
      ("ullman-ex2.08.wml",
        Program [
          ExprStm (Binop(RPlus, Binop(RMinus, Real 3.0, Real 4.5), Real 6.7)),
          ExprStm (Binop(ITimes, Binop(IDiv, Int 43, Binop(Mod, Int 8, Int 3)), Int 5))
        ]),
      ("ullman-ex2.09.wml",
        Program [
          ExprStm (Binop(Cat, Str "house", Str "cat")),
          ExprStm (Binop(Cat, Str "linoleum", Str ""))
        ]),
      ("ullman-ex2.10.wml",
        Program [
          ExprStm (Binop(ILt, Int 2, Binop(IPlus, Int 1, Int 3))),
          ExprStm (Binop(RLt, Real 2.1, Binop(RPlus, Real 1.5, Real 3.7)))
        ]),
      ("ullman-ex2.11.wml",
        Program [
          ExprStm (Orelse (Binop(ILt, Int 1, Int 2), Binop(IGt, Int 3, Int 4))),
          ExprStm (Andalso (Binop(ILt, Int 1, Int 2), Binop(IGt, Int 3, Int 4)))
        ]),
      ("ullman-ex2.12.wml",
        Program [
          ExprStm (App (Ident "not", Binop(ILt, Int 1, Int 2)))
        ]),
      ("ullman-ex2.13.wml",
        Program [
          ExprStm(Cond(Binop(ILt, Int 1, Int 2),
                       Binop(IPlus, Int 3, Int 4),
                       Binop(IPlus, Int 5, Int 6)))
          ]),
      ("ullman-ex2.23a.wml",
        Program [
          Dec(ValDec(PIdent "pi", Real 3.1415927)),
          Dec(ValDec(PIdent "radius", Real 4.0)),
          ExprStm(Binop(RTimes, Binop(RTimes, Ident "pi", Ident "radius"), Ident "radius"))
        ]),
      ("ullman-ex2.23b.wml",
        Program [
          Dec(ValDec(PIdent "pi", Real 3.1415927)),
          Dec(ValDec(PIdent "radius", Real 4.0)),
          Dec(ValDec(PIdent "area",
                     Binop(RTimes, Binop(RTimes, Ident "pi", Ident "radius"), Ident "radius"))),
          ExprStm(Ident "area")
        ]),
      ("ullman-ex2.25.wml",
        Program [
          Dec(ValDec(PIdent "t", Tuple [Int 4, Real 5.0, Str "size"]))
        ]),
      ("ullman-ex2.26.wml",
        Program [
          ExprStm(Tuple [Int 1, Int 2, Int 3, Int 4]),
          ExprStm(Tuple [Int 1, Tuple [Int 2, Real 3.0]]),
          ExprStm(Int 1)
        ]),
      ("ullman-ex2.27.wml",
        Program [
          Dec(ValDec(PIdent "t", Tuple [Int 4, Real 5.0, Str "six"])),
          ExprStm(App( Sel 1, Ident "t")),
          ExprStm(App( Sel 3, Ident "t"))
        ]),
      ("ullman-ex2.28.wml",
        Program [
          ExprStm(Binop(Cons, Int 1, Binop(Cons, Int 2, Binop(Cons, Int 3, Nil))))
        ]),
      ("ullman-ex2.29.wml",
        Program [
          ExprStm(Binop(Cons, Str "a", Nil))
        ]),
      ("ullman-ex2.33.wml",
        Program [
          ExprStm(Binop(Cons, Int 1,
                       Binop(Cons, Int 2,
                            Binop(Cons, Int 3,
                                 Nil))))
        ]),
      ("ullman-p2.1.1.wml",
        Program [
          ExprStm(Binop(IPlus, Int 1, Binop(ITimes, Int 2, Int 3))),
          ExprStm(Binop(RMinus, Real 5.0, Binop(RDiv, Real 4.2, Real 1.4))),
          ExprStm(Binop(Mod, Binop(IDiv, Int 11, Int 2), Int 3)),
          ExprStm(Binop(Cat, Binop(Cat, Str "foo", Str "bar"), Str "")),
          ExprStm(Orelse(Binop(IGt, Int 3, Int 4),
                         Andalso(Binop(ILt, Int 5, Int 6),
                                 App( Ident "not", Binop(Ne, Int 7, Int 8))))),
          ExprStm(Cond(Binop(ILt, Int 6, Int 10),
                       Real 6.0,
                       Real 10.0))
        ]),
      ("ullman-s2.4.4.wml",
        Program [
          ExprStm(Nil),
          ExprStm(Nil),
          Dec(ValDec(PIdent "L", Binop(Cons, 
            Int 2, Binop(Cons, Int 3, Binop(Cons, Int 4, Nil))))),
          Dec(ValDec(PIdent "M", Binop(Cons, Int 5, Nil))),
          ExprStm(App( Ident "hd", Ident "L")),
          ExprStm(App( Ident "tl", Ident "L")),
          ExprStm(App( Ident "hd", Ident "M")),
          ExprStm(App( Ident "tl", Ident "M")),
          ExprStm(Binop(Append, Binop(Cons, Int 1, Binop(Cons, Int 2, Nil)),
                         Binop(Cons, Int 3, Binop(Cons, Int 4, Nil)))),
          ExprStm(Binop(Cons, Int 2, Binop(Cons, Int 3, Binop(Cons, Int 4, Nil)))),
          ExprStm(Binop(Cons, Real 2.0, Nil))
        ]),
      ("ullman-ex3.1.wml",
        Program [
          Dec(FunDec("upper", [PIdent "c"],
                     App( Ident "chr",
                         Binop(IMinus, App( Ident "ord", Ident "c"),
                               Int 32)))),
          ExprStm(App( Ident "upper", Char #"a"))
        ]),
      ("ullman-ex3.2.wml",
        Program [
          Dec(FunDec("square", [PIdent "x"], Binop(ITimes, Ident "x", Ident "x")))
        ]),
      ("ullman-ex3.6.wml",
        Program [
          Dec(FunDec("max3", [PIdent "a", PIdent "b", PIdent "c"],
                     Cond(Binop(IGt, Ident "a", Ident "b"),
                          Cond(Binop(IGt, Ident "a", Ident "c"),
                               Ident "a",
                               Ident "c"),
                          Cond(Binop(IGt, Ident "b", Ident "c"),
                               Ident "b",
                               Ident "c")))),
          ExprStm(App( App( App( Ident "max3", Int 1), Int 2), Int 3))
        ]),
      ("ullman-ex3.5.wml",
        Program [
          Dec(FunDec("square", [PIdent "x"], Binop(ITimes, Ident "x", Ident "x"))),
          Dec(ValDec(PIdent "x", Int 3)),
          Dec(ValDec(PIdent "y", Int 4)),
          ExprStm(Binop(IPlus, App( Ident "square", Ident "x"), Ident "y")),
          ExprStm(App( Ident "square", Binop(IPlus, Ident "x", Ident "y")))
        ])
    ]
  end

  val testDir = TU.testfilesDir ^ "/parse"

  fun makeParseableTests dir =
    (dir, map assertPgmParses (TestsUtility.wmlFiles dir))

  fun parseableCorrect() = ("correct parsings",
    map assertPgmParsesCorrectly 
        (map (fn (f, p) => (testDir ^ "/ullman/" ^ f, p)) parsings)
  )

  fun unparseable() = ("unparseable",
    map 
      assertPgmFailsParse 
      (TestsUtility.wmlFiles (testDir ^ "/unparseable"))
  )

  fun test_suites() = 
    parseableCorrect() ::
    (map 
       (makeParseableTests o (fn d => testDir ^ "/" ^ d)) 
       ["synth", "ullman", "paulson"]
    ) @
    [unparseable()]

end

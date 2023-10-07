(*  COMP 323:  Parser for WML.
*
*  N. Danner
*
*)

structure Parse =
struct

  structure T = Tokens

  type stream = MakeLex.stream
  val lex = MakeLex.makelex()

  type 'a parse_result = 'a*stream
  type 'a parser = stream -> 'a parse_result
             
  exception NoParse of string
  exception ExtraTokens of stream

  (*  ****************************************
  *   Association functions.
  *   ****************************************
  *)

  (*  leftAssocConstr lhs [(r_0, e_0),...,(r_{n-1}, e_{n-1})] =
  *     r_{n-1}(...r_1(r_0(e, e_0), e_1)..., e_{n-1}).
  *
  *   E.g., leftAssocConstr 1 [(+, 2), (+, 3), (+, 4)] = ((1 + 2) + 3) + 4
  *)
  fun leftAssocConstr
      (lhs : Ast.exp)
      (rrs : ((Ast.exp*Ast.exp -> Ast.exp) * Ast.exp) list) : Ast.exp =
    case rrs of
         [] => lhs
       | (rator, rhs) :: rrs =>
           leftAssocConstr (rator(lhs, rhs)) rrs

  fun leftAssoc
      (lhs : Ast.exp)
      (rrs : (Ast.binop * Ast.exp) list) : Ast.exp =
    case rrs of
         [] => lhs
       | (rator, rhs) :: rrs => leftAssoc (Ast.Binop(rator, lhs, rhs)) rrs

  (*  rightAssoc lhs [(r_0, e_0),...,(r_{n-1}, e_{n-1})] =
  *     r_0(lhs, e_0(...r_{n-2}(e_{n-3}, r_{n-1}(e_{n-2}, e_{n-1}))...))
  *
  *   E.g., rightAssoc xs [(::, ys), (:: zs)] = xs :: (ys :: zs).
  *)
  fun rightAssoc
      (lhs : Ast.exp)
      (rrs : (Ast.binop * Ast.exp) list) : Ast.exp =
    case rrs of
         [] => lhs
       | (rator, rhs) :: rrs => Ast.Binop(rator, lhs, rightAssoc rhs rrs)

  fun rightAssoc1
      (rator : Ast.exp * Ast.exp -> Ast.exp)
      (lhs : Ast.exp) 
      (rrs : Ast.exp list) : Ast.exp =
    case rrs of
         [] => lhs
       | rhs :: rrs => rator(lhs, rightAssoc1 rator rhs rrs)

  (*  ****************************************
  *   Expression parsing functions.
  *   ****************************************
  *)

  fun parsePatterns (strm : stream) : (Ast.pat list) * stream =
    case lex strm of
         (T.ID x, strm) => 
         let
           val (ps, strm) = parsePatterns strm
         in
           (Ast.PIdent x :: ps, strm)
         end
       | (T.WILD, strm) => 
         let
           val (ps, strm) = parsePatterns strm
         in
           (Ast.PWild :: ps, strm)
         end
       | _ => ([], strm)

  fun parse_start (strm : stream) = parse_fn strm

  (*  Productions for ML's exp category.
  *)
  and parse_fn (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_fn")
  in
    case lex strm of
         (T.FN, strm') =>
         (
           let
             val (ps, strm') = parsePatterns strm'
             val (T.DARROW, strm') = lex strm'
             val (e, strm') = parse_start strm'
           in
             (Ast.Lambda(ps, e), strm')
           end
           handle Bind => raise NoParse "parse_fn"
         )
       | _ => parse_cond strm
  end

  and parse_cond (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_cond")
  in
    case lex strm of
         (T.IF, strm') =>
         (
           let
             val (e, strm') = parse_start strm'
             val (T.THEN, strm') = lex strm'
             val (e0, strm') = parse_start strm'
             val (T.ELSE, strm') = lex strm'
             val (e1, strm') = parse_start strm'
           in
             (Ast.Cond(e, e0, e1), strm')
           end
           handle Bind => raise NoParse "parse_cond"
         )
       | _ => parse_orelse strm
  end

  and parse_orelse (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_orelse")
    val (lhs, strm) = parse_andalso strm
    val (rrs, strm) = parse_orelse' strm
  in
    (leftAssocConstr lhs (map (fn rhs => (Ast.Orelse, rhs)) rrs), strm)
  end

  and parse_orelse' (strm : stream) : Ast.exp list parse_result =
    case lex strm of
         (T.ORELSE, strm') =>
         let
           val (e, strm') = parse_andalso strm'
           val (es, strm') = parse_orelse' strm'
         in
           (e :: es, strm')
         end
       | _ => ([], strm)

  and parse_andalso (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_andalso")
    val (lhs, strm) = parse_comp strm
    val (rrs, strm) = parse_andalso' strm
  in
    (leftAssocConstr lhs (map (fn rhs => (Ast.Andalso, rhs)) rrs), strm)
  end

  and parse_andalso' (strm : stream) : Ast.exp list parse_result =
    case lex strm of
         (T.ANDALSO, strm') =>
         let
           val (e, strm') = parse_comp strm'
           val (es, strm') = parse_andalso' strm'
         in
           (e :: es, strm')
         end
       | _ => ([], strm)


  (*  ML's infexp (infix expression) category.
  *)
  and parse_comp (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_comp")

    val (lhs, strm) = parse_consapp strm

    fun combine rator strm = 
    let
      val (rhs, strm) = parse_consapp strm
    in
      (Ast.Binop(rator, lhs, rhs), strm)
    end
  in
    case lex strm of
         (T.EQ, strm') => combine Ast.Eq strm'
       | (T.NE, strm') => combine Ast.Ne strm'
       | (T.LT, strm') => combine Ast.ILt strm'
       | (T.LE, strm') => combine Ast.ILe strm'
       | (T.GT, strm') => combine Ast.IGt strm'
       | (T.GE, strm') => combine Ast.IGe strm'
       | (T.LTPOUND, strm') => combine Ast.RLt strm'
       | (T.LEPOUND, strm') => combine Ast.RLe strm'
       | (T.GTPOUND, strm') => combine Ast.RGt strm'
       | (T.GEPOUND, strm') => combine Ast.RGe strm'
       | _ => (lhs, strm)
  end

  and parse_consapp (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_consapp")
    val (lhs, strm') = parse_pm strm
    val (rrs, strm') = parse_consapp' strm'
  in
    (rightAssoc lhs rrs, strm')
  end

  and parse_consapp' (strm : stream) : (Ast.binop*Ast.exp) list parse_result =
    case lex strm of
         (T.DCOLON, strm') => parse_consapp'' Ast.Cons strm'
       | (T.AT, strm') => parse_consapp'' Ast.Append strm'
       | _ => ([], strm)

  and parse_consapp'' (rator : Ast.binop) (strm : stream) :
      (Ast.binop*Ast.exp) list parse_result =
  let
    val (e, strm') = parse_pm strm
    val (rrs, strm') = parse_consapp' strm'
  in
    ((rator, e) :: rrs, strm')
  end

  and parse_pm (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_pm")
    val (lhs, strm') = parse_md strm
    val (rrs, strm') = parse_pm' strm'
  in
    (leftAssoc lhs rrs, strm')
  end

  and parse_pm' (strm : stream) : (Ast.binop*Ast.exp) list parse_result =
    case lex strm of
         (T.PLUS, strm') => parse_pm'' Ast.IPlus strm'
       | (T.MINUS, strm') => parse_pm'' Ast.IMinus strm'
       | (T.PLUSPOUND, strm') => parse_pm'' Ast.RPlus strm'
       | (T.MINUSPOUND, strm') => parse_pm'' Ast.RMinus strm'
       | (T.CAT, strm') => parse_pm'' Ast.Cat strm'
       | _ => ([], strm)

  and parse_pm'' (rator : Ast.binop) (strm : stream) : 
      (Ast.binop*Ast.exp) list parse_result=
  let
    val (e, strm') = parse_md strm
    val (rrs, strm') = parse_pm' strm'
  in
    ((rator, e) :: rrs, strm')
  end

  and parse_md (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_md")
    val (lhs, strm') = parse_app strm
    val (rrs, strm') = parse_md' strm'
  in
    (leftAssoc lhs rrs, strm')
  end

  and parse_md' (strm : stream) : (Ast.binop*Ast.exp) list parse_result =
    case lex strm of
         (T.TIMES, strm') => parse_md'' Ast.ITimes strm'
       | (T.DIV, strm') => parse_md'' Ast.IDiv strm'
       | (T.TIMESPOUND, strm') => parse_md'' Ast.RTimes strm'
       | (T.DIVPOUND, strm') => parse_md'' Ast.RDiv strm'
       | (T.MOD, strm') => parse_md'' Ast.Mod strm'
       | _ => ([], strm)

  and parse_md'' (rator : Ast.binop) (strm : stream) :
      (Ast.binop*Ast.exp) list parse_result =
  let
    val (e, strm') = parse_app strm
    val (rrs, strm') = parse_md' strm'
  in
    ((rator, e) :: rrs, strm')
  end

  and parse_app (strm : stream) : Ast.exp parse_result =
  let
    val _ = Util.dbg_printnl (fn () => "parse_app")
    val (rator, strm') = parse_atomic strm
    val (rands, strm') = parse_atomics strm'
  in
    (leftAssocConstr rator (map (fn e => (Ast.App, e)) rands), strm')
  end

  (*  ML's atexp (atomic expression) category.
  *)
  and parse_atomic (strm : stream) : Ast.exp parse_result =
    parse_bracks strm
    handle NoParse _ =>
      parse_parens strm
      handle NoParse _ =>
        parse_let strm
        handle NoParse _ => parse_lit_id_sel strm

  and parse_atomics (strm : stream) : Ast.exp list parse_result =
  let
    val (e, strm') = parse_atomic strm
    val (es, strm') = parse_atomics strm'
  in
    (e :: es, strm')
  end
  handle NoParse _ => ([], strm)

  and parseWithCommas (strm : stream) : Ast.exp list parse_result =
  let
    val (e, strm') = parse_start strm
    val (es, strm') = parseWithCommas' strm'
  in
    (e :: es, strm')
  end
  handle NoParse _ => ([], strm)

  and parseWithCommas' (strm : stream) : Ast.exp list parse_result =
  let
    val (T.COMMA, strm') = lex strm
    val (e, strm') = parse_start strm'
    val (es, strm') = parseWithCommas' strm'
  in
    (e :: es, strm')
  end
  handle Bind => ([], strm)

  and parse_bracks (strm : stream) : Ast.exp parse_result =
    case lex strm of
         (T.LBRACK, strm') =>
         (
           let
             val (es, strm') = parseWithCommas strm'
             val (T.RBRACK, strm') = lex strm'
           in
           (
             foldr
               (fn (h, t) => (Ast.Binop(Ast.Cons, h, t)))
               Ast.Nil
               es,
             strm'
           )
           end
           handle Bind => raise NoParse "parse_bracks: read [ without ]"
         )
       | _ => raise NoParse "parse_bracks"

  and parse_parens (strm : stream) : Ast.exp parse_result =
    case lex strm of
         (T.LP, strm') =>
         (
           let
             val (es, strm') = parseWithCommas strm'
             val (T.RP, strm') = lex strm'
           in
             case es of
                  [] => (Ast.Triv, strm')
                | [e] => (e, strm')
                | _ => (Ast.Tuple es, strm')
           end
           handle Bind => raise NoParse "parse_parens: read ( without )"
         )
       | _ => raise NoParse "parse_parens"


  and parse_let (strm : stream) : Ast.exp parse_result =
  let
    val (T.LET, strm') = lex strm
    val (ds, strm') = parseDecsMaybeSemis strm'
    val (T.IN, strm') = lex strm'
    val (e, strm') = parse_start strm'
    val (T.END, strm') = lex strm'
  in
    (Ast.Let(ds, e), strm')
  end
  handle Bind => raise NoParse "parse_let"

  and parse_lit_id_sel (strm : stream) : Ast.exp parse_result =
    case lex strm of
         (T.INT n, strm') => (Ast.Int n, strm')
       | (T.REAL x, strm') => (Ast.Real x, strm')
       | (T.TRUE, strm') => (Ast.Bool true, strm')
       | (T.FALSE, strm') => (Ast.Bool false, strm')
       | (T.CHAR c, strm') => (Ast.Char c, strm')
       | (T.STRING s, strm') => (Ast.Str s, strm')
       | (T.ID x, strm') => (Ast.Ident x, strm')
       | (T.SEL n, strm') => (Ast.Sel n, strm')
       | (T.NIL, strm') => (Ast.Nil, strm')
       | _ => raise NoParse "parse_lit_id_sel"


  (*  ****************************************
  *   Program parsing functions.
  *
  *   These must still be part of the mutual recursion used to define the
  *   expression parsers, becaues both programs and let expressions depend on
  *   declarations.
  *   ****************************************
  *)

  (*  Declaration parser.
  *)
  and parse_decl (strm : stream) : Ast.dec parse_result =
    case lex strm of
         (T.VAL, strm') => parse_valdec strm
       | (T.FUN, strm') => parse_fundec strm
       | _ => raise NoParse "parse_decl"

  and parseDecMaybeSemi (strm : stream) : Ast.dec parse_result =
  let
    val (d, strm') = parse_decl strm
  in
    case lex strm' of
         (T.SEMI, strm') => (d, strm')
       | _ => (d, strm')
  end

  and parseDecsMaybeSemis (strm : stream) : Ast.dec list parse_result =
  let
    val (d, strm') = parseDecMaybeSemi strm
    val (ds, strm') = parseDecsMaybeSemis strm'
  in
    (d :: ds, strm')
  end
  handle NoParse _ => ([], strm)

  (*  Value declaration parser.
  *)
  and parse_valdec (strm : stream) : Ast.dec parse_result =
  let
    val (T.VAL, strm') = lex strm
  in
    case lex strm' of
         (T.WILD, strm') =>
         (
           let
             val (T.EQ, strm') = lex strm'
             val (e, strm') = parse_start strm'
           in
             (Ast.ValDec(Ast.PWild, e), strm')
           end
           handle Bind => raise NoParse "parse_valdec: expected WILD EQ"
         )
       | (T.ID x, strm') =>
         (
           let
             val (T.EQ, strm') = lex strm'
             val (e, strm') = parse_start strm'
           in
             (Ast.ValDec(Ast.PIdent x, e), strm')
           end
           handle Bind => raise NoParse "parse_valdec: expected ID EQ"
         )
       | _ => raise NoParse "parse_valdec: expected WILD or ID"
  end
  handle Bind => raise NoParse "parse_valdec: expected VAL"

  (*  Function definition parser.
  *)
  and parse_fundec (strm : stream) : Ast.dec parse_result =
  let
    val (T.FUN, strm') = lex strm
    val (T.ID f, strm') = lex strm'
    val (ps, strm') = parsePatterns strm'
    val (T.EQ, strm') = lex strm'
    val (e, strm') = parse_start strm'
  in
    (Ast.FunDec(f, ps, e), strm')
  end
  handle Bind => raise NoParse "parse_fundec: expected FUN ID EQ exp"


  (*  Expression statement parser.
  *)
  fun parse_exp (stm : stream) : Ast.exp parse_result =
    parse_start stm

  (*  Statement parser.
  *)
  fun parse_stm (strm : stream) : Ast.stm parse_result =
  (
    case lex strm of
         (T.VAL, _) =>
         let
           val (d, strm') = parse_valdec strm
           val (T.SEMI, strm') = lex strm'
         in
           (Ast.Dec d, strm')
         end
       | (T.FUN, _) =>
         let
           val (d, strm') = parse_fundec strm
           val (T.SEMI, strm') = lex strm'
         in
           (Ast.Dec d, strm')
         end
       | (_, _) =>
         let
           val (e, strm') = parse_exp strm
           val (T.SEMI, strm') = lex strm'
         in
           (Ast.ExprStm e, strm')
         end
  )
  handle Bind => raise NoParse "parse_stm: expected VAL, FUN, or exp ;"

  fun parse_stms (strm : stream) : Ast.stm list parse_result =
  let
    val (s, strm') = parse_stm strm
    val (ss, strm') = parse_stms strm'
  in
    (s :: ss, strm')
  end
  handle NoParse _ => ([], strm)

  (*  Program parser.
  *)
  fun parse_pgm (strm : stream) : Ast.pgm parse_result =
  let
    val (ss, strm') = parse_stms strm
  in
    (Ast.Program ss, strm')
  end

  (* ****************************************
  *  Client-visible parsing functions.
  *  ****************************************
  *)

  fun parseExp (strm : stream) : Ast.exp =
  let
    val (e, s) = parse_exp strm
  in
    case lex s of
         (T.EOF, _) => e
       | _ => raise ExtraTokens s
  end

  fun parsePgm (strm : stream) : Ast.pgm =
  let
    val (p, s) = parse_pgm strm
  in
    case lex s of
         (T.EOF, _) => p
       | _ => raise ExtraTokens s
  end

end

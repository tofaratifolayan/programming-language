(*  COMP 323:  Definitional interpreter for WML.
*
*  N. Danner
*)

structure Interp =
struct

  (* ********************
  *  Convenience.
  *  ********************
  *)
  val dbg_print = Util.dbg_print
  val dbg_printnl = Util.dbg_printnl
  val $ = Util.$
  infix 0 $

  (* ********************
  *  Exceptions.
  *  ********************
  *)
  exception InterpError of string

  (* ********************
  *  Values.  Most are self-explanatory.
  *
  *  RecClosure(f, [x_k,...,x_{n-1}], e, rho, [x_0,...,x_{k-1}])
  *  represents the (possibly) recursive function f x_0 ... x_{n-1} that has
  *  been partially applied to arguments for x_0,...,x_{k-1}.
  *
  *  Builtin id represents the built-in function id.  Each built-in function
  *  is associated to a function that maps values to values, specified by the
  *  baseEnv environment.
  *  ********************
  *)
  datatype value = Int of int | Real of real | Char of char | Str of string
                 | Bool of bool | Triv
                 | Tuple of value list | List of value list

  (*  valueToString v = a string representation of v.
  *)
  fun valueToString (v : value) : string =
    case v of
         Int n => Int.toString n
       | Real x => Real.toString x
       | Char c => "#\"" ^ Char.toString c ^ "\""
       | Str s => "\"" ^ String.toString s ^ "\""
       | Bool b => Bool.toString b
       | Triv => "()"
       | Tuple vs => 
           ListFormat.fmt {init="(", final=")", sep=",", fmt=valueToString} vs
       | List vs =>
           ListFormat.listToString valueToString vs

  (*  evalExp e = v, where _ ├ e ↓ v.
  *)
  fun evalExp (e : Ast.exp) : value =
    raise Fail "evalExp unimplemented."

  (*  execPgm p = ().
  *
  *  Effect:  the program p is executed under the base environment.  Each
  *  statement in p is executed in order.  A value declaration is executed by
  *  evaluating its RHS and then adding the binding to the current
  *  environment.  A function declaration is executed by adding the function
  *  binding to the current environment as an unapplied RecClosure.  An
  *  expression statement is executed by evaluating the expression to a value
  *  v, then printing a string representation of v to the terminal using
  *  valueToString.
  *)
  fun execPgm (Ast.Program stms : Ast.pgm) : unit =
    raise Fail "execPgm unimplemented."

end

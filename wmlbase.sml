(* WML definitions for SML.
*)

(* Real arithmetic operators.  Because we are messing around with
* already-defined symbols, make sure to use functions from the appropriate
* structure.
*)

infix 4 <#
fun (x : real) <# (y : real) : bool = Real.<(x, y)

infix 4 >#
fun (x : real) ># (y : real) : bool = Real.>(x, y)

infix 4 <=#
fun (x : real) <=# (y : real) : bool = Real.<=(x, y)

infix 4 >=#
fun (x : real) >=# (y : real) : bool = Real.>=(x, y)

infix 6 +#
fun (x : real) +# (y : real) : real = Real.+(x, y)

infix 6 -#
fun (x : real) -# (y : real) : real = Real.-(x, y)

infix 7 *#
fun (x : real) *# (y : real) : real = Real.*(x, y)

infix 7 /#
fun (x : real) /# (y : real) : real = Real./(x, y)

(* Integer arithmetic.  We "redefine" the usual symbols to ensure that they
* are not overloaded at real.
*)
fun (x : int) + (y : int) : int = Int.+(x, y)
fun (x : int) - (y : int) : int = Int.-(x, y)
fun (x : int) * (y : int) : int = Int.*(x, y)
fun (x : int) / (y : int) : int = Int.div(x, y)

infix 7 %
fun (x : int) % (y : int) : int = Int.mod(x, y)

(* Printing functions.
*)

fun printInt n = print (Int.toString n ^ "\n") ;
fun printReal x = print (Real.toString x ^ "\n") ;
fun printBool n = print (Bool.toString n ^ "\n") ;
fun printChar c = print (Char.toString c ^ "\n") ;
fun printString s = print (s ^ "\n")


(*  COMP 323 sample code:  a structure for environments.
*
*  An environment is a map with domain Ast.ident.
*
*  N. Danner
*)

structure Env =
struct

  structure M = SplayMapFn(Ast.IdentKey)

  exception Domain of Ast.ident

  type 'a env = 'a M.map

  val empty : 'a env = M.empty

  fun update (rho : 'a env) (x : Ast.ident) (v : 'a) =
    M.insert(rho, x, v)

  fun updateMany (rho : 'a env) (kvs : (Ast.ident*'a) list) : 'a env =
    foldl M.insert' rho kvs

  fun get (rho : 'a env) (x : Ast.ident) =
    valOf (M.find(rho, x))
    handle Option => raise Domain x

  fun listItemsi (rho : 'a env) =
    M.listItemsi rho

end

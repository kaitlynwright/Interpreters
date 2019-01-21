(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

datatype term
  = AST_ID of string
  | AST_NUM of int
  | AST_BOOL of bool
  | AST_FUN of (string * term)
  | AST_APP of (term * term)
  | AST_SUCC
  | AST_PRED
  | AST_ISZERO
  | AST_IF of (term * term * term)

datatype result
  = RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term)
  | RES_CLOSURE of (result * env)
  and env = Env of (string -> term);

(*  An environment is a function string -> result.
 *  I've included code for the empty environment
 *  and code that produces the updated environment E[x : t].
 *)

exception UnboundID
datatype env = Env of (string -> term)
fun emptyenvFun  (x : string) : result = raise UnboundID;
val emptyenv = Env emptyenvFun

(*  update : (string -> result) -> string -> result -> string -> result  *)
fun update (Env e) (x : string) (ty : result) y = if x = y then ty else e y

(*  Here's a partial skeleton of interp : (env * term) -> result.
    I've done the first case for you
*)

fun lookup (Env f) i = f i

fun interp (env, AST_ID i)          = lazy_interp(lookup env i)
  | interp (env, AST_NUM n)         = RES_NUM(n)
  | interp (env, AST_BOOL b)        = RES_BOOL(b)
  | interp (env, AST_FUN (i,e))     = RES_FUN(i, e)
  | interp (env, AST_APP (e1,e2))   = ( case interp(env, e1) of 
                                        RES_PRED => ( case interp(env, e2) of 
                                              RES_NUM(0) => RES_NUM(0) 
                                              |  RES_NUM(n) => RES_NUM(n-1)  
                                              | _ => RES_ERROR "not a valid type" )

                                        | RES_SUCC => ( case interp(env, e2) of 
                                              RES_NUM(n) => RES_NUM(n+1) 
                                              | _ => RES_ERROR "not valid type" )

                                        | RES_ISZERO => ( case interp(env, e2) of 
                                              RES_NUM(0) => RES_BOOL(true) 
                                              | RES_NUM(n) => RES_BOOL(false) 
                                              | _ => RES_ERROR "not valid type" )

                                        | RES_FUN (i, e) => interp(Env(update env i (interp(env, e2))), e )
                                        )
  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) = case interp(env, e1) of 
                                        RES_BOOL(false) => interp(env, e3)
                                        | RES_BOOL(true) => interp(env, e2)
                                        | _ => RES_ERROR "not valid type"


(*val numTest = interp, emptyenv, (fn x => x) 42)*)


(*interp(emptyenvFun, AST_APP(AST_SUCC, RES_NUM(42)))*)

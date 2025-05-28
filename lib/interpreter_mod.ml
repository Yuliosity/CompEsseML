module StrMap = Map.Make(String)

type env = int StrMap.t

open Ast

module type INTERP = sig
  val interp_expr : env -> Ast.expr -> int
  val interp : Ast.program -> int
end

module InterpLIntF (I : INTERP) = struct
  let interp {body} = I.interp_expr StrMap.empty body
  let interp_expr env = function
    | Prim0 Read -> read_int ()
    | Int i -> i
    | Prim1 {op = Neg; arg} -> -(I.interp_expr env arg)
    | Prim2 {op; l; r} -> begin
      let l = I.interp_expr env l in
      let r = I.interp_expr env r in
      match op with
        | Add -> l + r
        | Sub -> l - r
      end
    | _ -> failwith "Unreachable: interp_exp"
end

module rec InterpLInt : INTERP = InterpLIntF (InterpLInt)

let%test_unit "LInt" =
  let eval_expr e = InterpLInt.interp {body = e} in
  assert (eval_expr (Prim1 {op = Neg; arg = Int 10}) = -10);
  assert (eval_expr (Prim2 {op = Add; l = Int 32; r = Int 10}) = 42);
  let test3 = Prim2 {op = Sub; l = Int 52; r = Prim2 {op = Add; l = Int 5; r = Int 5}} in
  assert (eval_expr test3 = 42)

module rec InterpLVar : INTERP = struct
  module Base = InterpLIntF (InterpLVar)
  let rec interp_expr env = function
    | Var v -> StrMap.find v env
    | Let {v; e; body} ->
      let res = interp_expr env e in
      interp_expr (StrMap.add v res env) body
    | e -> Base.interp_expr env e
  let interp {body} = interp_expr StrMap.empty body
end

let%test_unit "LVar" =
  let eval_expr e = InterpLVar.interp {body = e} in
  assert (eval_expr (Prim2 {op = Add; l = Int 32; r = Int 10}) = 42);
  let test2 =
    Let {
      v = "aa";
      e = Prim2 {op = Add; l = Int 20; r = Int 1};
      body = Prim2 {op = Add; l = Var "aa"; r = Var "aa"}
    }
  in
  assert (eval_expr test2 = 42)

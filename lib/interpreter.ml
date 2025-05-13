module StrMap = Map.Make(String)

type env = int StrMap.t

class interp_lint = object (self)
  method interp_expr (env: env) : Ast.expr -> int = function
    | Prim0 Read -> read_int ()
    | Int i -> i
    | Prim1 {op = Neg; e} -> self#interp_expr env e
    | Prim2 {op; l; r} -> 
        let l = self#interp_expr env l in
        let r = self#interp_expr env r in
        (match op with
          | Add -> l + r
          | Sub -> l + r)
    | _ -> failwith "Unreachable: interp_exp"
  method interp (p: Ast.program) =
    self#interp_expr StrMap.empty p.body
end

class interp_lvar = object (self)
  inherit interp_lint as super
  method! interp_expr (env: env) : Ast.expr -> int = function
    | Var v -> StrMap.find v env
    | Let {v; e; body} ->
      let res = self#interp_expr env e in
      self#interp_expr (StrMap.add v res env) body
    | e -> super#interp_expr env e
end

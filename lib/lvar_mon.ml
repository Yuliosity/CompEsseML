module In = Ast

type atom = Int of int | Var of string

type prim1 = {op: Ast.prim1op; arg: atom}

type prim2 = {op: Ast.prim2op; l: atom; r: atom}

type expr =
  | Atom of atom
  | Prim0 of Ast.prim0
  | Prim1 of prim1
  | Prim2 of prim2
  | Let of {v: string; e: expr; body: expr}

type program = {body: expr}

let tmp i = "tmp_" ^ string_of_int i

let rec rco_expr i expr = match expr with
  | In.Var v -> Atom (Var v)
  | In.Int i -> Atom (Int i)
  | In.Prim0 p -> Prim0 p
  | In.Prim1 {op; arg} -> begin
    match rco_expr (i + 1) arg with
    | Atom arg -> Prim1 {op; arg}
    | e ->
      let v = tmp i in
      Let {v = tmp i; e; body = Prim1 {op; arg = Var v}}
    end
  | In.Prim2 {op; l; r} -> begin
    match rco_expr (i + 1) l, rco_expr (i + 2) r with
      | Atom l, Atom r -> Prim2 {op; l; r}
      | Atom l, r ->
        let v = tmp i in
        Let {v; e = r; body = Prim2 {op; l; r = Var v}}
      | l, Atom r ->
        let v = tmp i in
        Let {v; e = l; body = Prim2 {op; l = Var v; r}}
      | l, r ->
        let v1 = tmp i in
        let v2 = tmp (i + 1) in
        Let {v = v1; e = l; body =
          Let {v = v2; e = r; body = Prim2 {op; l = Var v1; r = Var v2}}}
    end
  | In.Let {v; e; body} ->
    Let {v; e = rco_expr i e; body = rco_expr i body}

(* Exercise 2.3 *)
let remove_complex_operands = rco_expr 1

let%test "(- (- (- x)))" =
  let e = In.Prim1 {
    op = In.Neg; arg = In.Prim1 {
      op = In.Neg; arg = In.Prim1 {op = In.Neg; arg = In.Var "x"}
    }
  } in
  let expect = Let {
    v = "tmp_1";
    e = Let {
      v = "tmp_2";
      e = Prim1 {op = Ast.Neg; arg = Var "x"};
      body = Prim1 {op = Ast.Neg; arg = Var "tmp_2"}
    };
    body = Prim1 {op = Ast.Neg; arg = Var "tmp_1"}
  } in
  remove_complex_operands e = expect

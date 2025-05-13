open Ast

module StrMap = Map.Make(String)

type env = int StrMap.t

(* Exercise 2.1 *)
let rec uniquify_expr env = 
  let mangle v i = v ^ "_" ^ string_of_int i in
  function
  | Var v -> Var (mangle v (StrMap.find v env))
  | Prim1 {op; e} -> Prim1 {op; e = uniquify_expr env e}
  | Prim2 {op; l; r} -> Prim2 {op; l = uniquify_expr env l; r = uniquify_expr env r}
  | Let {v; e; body} ->
    let i = match StrMap.find_opt v env with
      | None -> 1
      | Some i -> i + 1
    in
    let env = StrMap.add v i env in
    Let {
      v = mangle v i;
      e = uniquify_expr env e;
      body = uniquify_expr env body
    }
  | e -> e

let uniquify {body} = {body = uniquify_expr StrMap.empty body}

(* Exercise 2.2 *)
let%test_unit "" =
  let interp = new Interpreter.interp_lvar in
  let test_run str =
    let pr = Parser.program_of_string str in
    let pr2 = uniquify pr in
    assert (interp#interp pr = interp#interp pr2)
  in
  test_run "(let ((x 32)) (+ (let ((x 10)) x) x))";
  test_run {|
    (let ((x 5))
         (+ (let ((y 6))  (+ x y))
            (let ((y 10)) (+ x y))))
  |}
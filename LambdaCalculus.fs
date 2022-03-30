module LambdaCalculus

open Absyn

let rec freeVars =
  function
 | Var x -> Set.singleton x
 | Lam (x, t) -> freeVars t |> Set.remove x
 | App (t1, t2) -> freeVars t1 |> Set.union (freeVars t2)

let alpha e =
  let rec alpha' vars e' =
    match e' with
    | Var x ->
      match vars |> Map.tryFind x with
      | None -> Var x
      | Some y -> Var y
    | Lam (x, t) ->
      let prime = String.replicate (vars |> Map.count) "'"
      let y = $"{x}{prime}"
      Lam (y, alpha' (vars |> Map.add x y) t)
    | App (t1, t2) -> App(alpha' vars t1, alpha' vars t2)
  alpha' Map.empty e

let substitute x t e =
  let rec substitute' x t e =
    match e with
    | Var y when x = y -> t
    | Lam (y, t') when y <> x && not (freeVars t |> Set.contains y) ->
      Lam (y, substitute' x t t')
    | App (t1, t2) -> App((substitute' x t t1), (substitute' x t t2))
    | _ -> e
  substitute' x t (alpha e)

let rec cbn e =
  match e with
  | App (t1, t2) ->
    let t1' = cbn t1
    match t1' with
    | Lam (x, e') -> cbn (substitute x t2 e')
    | _ -> App(t1', t2)
  | _ -> e

/// Reduces a term to normal form
/// Sestoft, P.: Demonstrating Lambda Calculus Reduction (2001)
let rec nor e =
  match e with
  | Lam (x, e) -> Lam(x, nor e)
  | App (t1, t2) ->
    let t1' = cbn t1
    match t1' with
    | Lam (x, e) -> nor (substitute x t2 e)
    | _ -> App (t1', nor t2)
  | _ -> e

let toString e =
  let parens x = $"({x})"
  let rec toString' prec e' =
    match e' with
    | Var x -> x
    | Lam (x, t) ->
      let s = $"\\{x}.{toString' 0 t}"
      if prec > 0 then parens s else s
    | App (t1, t2) ->
      let s = $"{toString' 1 t1} {toString' 2 t2}"
      if prec > 1 then parens s else s
  toString' 0 e
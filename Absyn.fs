module Absyn

type Term =
  Var of string
  | Lam of string * Term
  | App of Term * Term
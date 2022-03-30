#r "packages/FsLexYacc.Runtime/lib/netstandard2.0/FsLexYacc.Runtime.dll"

#load "Absyn.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "LambdaCalculus.fs"

open FSharp.Text.Lexing
open LambdaCalculus

let parse input =
  let lexbuf = LexBuffer<char>.FromString input
  Parser.start Lexer.tokenize lexbuf

parse @"(\f.\g.\h.f g(h h))(\x.\y.x)h(\x.x x)"
|> nor
|> toString
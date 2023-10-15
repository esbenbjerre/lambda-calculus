#r "nuget: FsLexYacc"

#load "Absyn.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "LambdaCalculus.fs"

open FSharp.Text.Lexing
open LambdaCalculus

let parse input =
  let lexbuf = LexBuffer<char>.FromString input
  Parser.start Lexer.tokenize lexbuf

// AND TRUE FALSE
"(\p.\q.p q p) (\x.\y.x) (\x.\y.y)"
|> parse
|> nor
|> toString
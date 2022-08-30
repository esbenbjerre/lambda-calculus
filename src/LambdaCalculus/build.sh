#!/bin/sh
dotnet tool restore
if [ ! -e "paket.lock" ]
then
    dotnet paket install
else
    dotnet paket restore
fi

dotnet packages/FsLexYacc/build/fslex/netcoreapp3.1/fslex.dll --module Lexer --unicode Lexer.fsl
dotnet packages/FsLexYacc/build/fsyacc/netcoreapp3.1/fsyacc.dll --module Parser Parser.fsy

dotnet restore
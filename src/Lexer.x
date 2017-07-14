{
module Lexer (Token (..), LexOut (..), scan) where

import AST (Nat)
}

%wrapper "posn"

@char = [a-zA-Z_]
@digit = [0-9]

tokens :-

  $white+                               ;
  "#".*                                 ;

  ":"                                   { \p s -> lexOut p LColon }
  "="                                   { \p s -> lexOut p LEqual }

  "jmp"                                 { \p s -> lexOut p LJMP }
  "jif"                                 { \p s -> lexOut p LJIF }
  "read"                                { \p s -> lexOut p LRead }
  "print"                               { \p s -> lexOut p LPrint }

  "("                                   { \p s -> lexOut p LLParen }
  ")"                                   { \p s -> lexOut p LRParen }
  "+"                                   { \p s -> lexOut p LPlus }
  "-"                                   { \p s -> lexOut p LMinus }
  "*"                                   { \p s -> lexOut p LTimes }
  "/"                                   { \p s -> lexOut p LDiv }

  "|"                                   { \p s -> lexOut p LOr }
  "&"                                   { \p s -> lexOut p LAnd }
  "<"                                   { \p s -> lexOut p LLT }
  ">"                                   { \p s -> lexOut p LGT }

  "["                                   { \p s -> lexOut p LLSBracket }
  "]"                                   { \p s -> lexOut p LRSBracket }

  @digit+                               { \p s -> lexOut p (LNumber (read s)) }
  @char (@char | @digit)*               { \p s -> lexOut p (LSym s) }

{
data Token = LColon
           | LEqual
           | LJMP
           | LJIF
           | LRead
           | LPrint
           | LLParen
           | LRParen
           | LPlus
           | LMinus
           | LTimes
           | LDiv
           | LOr
           | LAnd
           | LLT
           | LGT
           | LLSBracket
           | LRSBracket
           | LNumber Nat
           | LSym String
           deriving (Eq, Show)

data LexOut = LexOut { offset :: Int
                     , line :: Int
                     , column :: Int
                     , getToken :: Token }
              deriving (Eq, Show)

lexOut (AlexPn offset line col) tok = LexOut offset line col tok

scan = alexScanTokens
}


module AST where

import Data.Word

-- TODO: Use arbitrary precision.
type Nat = Word64


type Program = [Statement]

data Statement = Label String
               | Assign Arith Arith
               | JMP Value
               | JIF Cond Value
               | Read Arith
               | Print Value

data Cond = And Cond Cond
          | Or Cond Cond
          | CLT Arith Arith
          | CEQ Arith Arith
          | CGT Arith Arith

data Arith = Val Value
           | Add Arith Arith
           | Sub Arith Arith
           | Mult Arith Arith
           | Div Arith Arith

data Value = Num Nat | Ref Arith | Location String

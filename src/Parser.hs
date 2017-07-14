module Parser where

import AST
import Lexer

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

number :: Parser Nat
number = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LNumber n) = Just n
    match' _ = Nothing

sym :: Parser String
sym = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LSym name) = Just name
    match' _ = Nothing

pos :: (SourcePos -> LexOut -> [LexOut] -> SourcePos)
pos oldPos (LexOut _ line col _) _ = newPos (sourceName oldPos) line col


-- * Grammar

program :: Parser Program
program = many statement


statement :: Parser Statement
statement = labelP <|> assign <|> jump <|> jumpIf <|> readP <|> printP

labelP = do lbl <- sym
            match LColon
            return (Label lbl)

assign = do ref <- reference
            match LEqual
            expr <- arith
            return (Assign ref expr)

jump = do match LJMP
          loc <- value
          return (JMP loc)

jumpIf =
  do match LJIF
     cond <- conditional
     loc <- value
     return (JIF cond loc)

readP = do match LRead
           ref <- reference
           return (Read ref)

printP = do match LPrint
            x <- value
            return (Print x)


arith :: Parser Arith
arith = do x <- prod
           xs <- many ((match LPlus *> fmap ((,) Add) prod)
                       <|> (match LMinus *> fmap ((,) Sub) prod))
           return (foldl (\x oy -> fst oy x (snd oy)) x xs)

prod = do x <- term
          xs <- many ((match LTimes *> fmap ((,) Mult) term)
                      <|> (match LDiv *> fmap ((,) Div) term))
          return (foldl (\x oy -> fst oy x (snd oy)) x xs)

term = (fmap Val value) <|> (match LLParen *> arith <* match LRParen)


conditional :: Parser Cond
conditional = do c <- andP
                 cs <- many (match LOr *> andP)
                 return (foldl Or c cs)

andP = do c <- comparison
          cs <- many (match LAnd *> comparison)
          return (foldl And c cs)

comparison = opNegParen <|> arithComp

opNegParen = do opNeg <- option id (match LNeg >> return Not)
                match LLParen
                cond <- conditional
                match LRParen
                return (opNeg cond)

arithComp = do x <- arith
               (c, y) <- ((match LLT *> fmap ((,) CLT) arith)
                           <|> (match LEqual *> fmap ((,) CEQ) arith)
                           <|> (match LGT *> fmap ((,) CGT) arith))
               return (c x y)


value :: Parser Value
value = (fmap Num number) <|> (fmap Ref reference) <|> (fmap Location sym)

reference :: Parser Arith
reference = do match LLSBracket
               ptr <- arith
               match LRSBracket
               return ptr

module Parser
( parseFormula
) where

import Data.Char
import Formula

data Token = TokOpAND
           | TokOpOR
           | TokOpNOT
           | TokTRUE
           | TokFALSE
           | TokLParen
           | TokRParen
           | TokVar String
           | TokEnd
    deriving (Show, Eq)
 
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | c == '~'  = TokOpNOT : tokenize cs
    | c == '&'  = TokOpAND : tokenize cs
    | c == '|'  = TokOpOR : tokenize cs
    | isAlpha c = variable c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

variable :: Char -> String -> [Token]
variable c cs
    | (c:name) == "TRUE" = TokTRUE : tokenize cs'
    | (c:name) == "FALSE" = TokFALSE : tokenize cs'
    | otherwise = TokVar (c:name) : tokenize cs'
  where (name, cs') = span isAlphaNum cs

---- parser ----


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts


expression :: [Token] -> (Formula, [Token])
expression toks = 
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
      	 TokOpAND ->
      	    let (exTree, toks'') = expression (accept toks') 
            in (AND termTree exTree, toks'')
         TokOpOR ->
      	    let (exTree, toks'') = expression (accept toks') 
            in (OR termTree exTree, toks'')
         _ -> (termTree, toks')

term :: [Token] -> (Formula, [Token])
term toks = 
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         TokOpAND ->
            let (termTree, toks'') = term (accept toks') 
            in (AND facTree termTree, toks'')
         TokOpOR ->
            let (termTree, toks'') = term (accept toks') 
            in (OR facTree termTree, toks'')
         _ -> (facTree, toks')

factor :: [Token] -> (Formula, [Token])
factor toks = 
   case lookAhead toks of
      (TokVar str) -> (Var str, accept toks)
      TokTRUE -> (Const True, accept toks)
      TokFALSE -> (Const False, accept toks)
      TokOpNOT -> 
            let (facTree, toks') = factor (accept toks) 
            in (NOT facTree, toks')
      TokLParen -> 
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks

parse :: [Token] -> Formula
parse toks = let (tree, toks') = expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

parseFormula :: String -> Formula
parseFormula = parse . tokenize

main = (print . parse . tokenize) "~A|(B&C)| FALSE"
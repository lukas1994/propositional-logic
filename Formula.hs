module Formula
( Formula(..)
, Context
, eval
) where

data Formula = Var String
             | Const Bool
             | NOT Formula
             | OR Formula Formula
             | AND Formula Formula
    deriving Eq

instance Show Formula where
    show (Var s) = show s
    show (Const b) = show b
    show (NOT f) = "~(" ++ (show f) ++ ")"
    show (OR f1 f2) = "(" ++ (show f1) ++ ") | (" ++ (show f2) ++ ")"
    show (AND f1 f2) = "(" ++ (show f1) ++ ") & (" ++ (show f2) ++ ")"

type Context = [(String, Bool)]

findInContext :: Context -> String -> Maybe Bool
findInContext [] _ = Nothing
findInContext ((k,v):cs) name
    | k == name = Just v
    | otherwise = findInContext cs name

eval :: Formula -> Context -> Bool
eval (Var name) c = case findInContext c name of
                      Just b -> b
                      Nothing -> error "assignment not complete"
eval (Const b) _ = b
eval (NOT f) c = not $ eval f c
eval (OR f1 f2) c = (eval f1 c) || (eval f2 c)
eval (AND f1 f2) c = (eval f1 c) && (eval f2 c)

{-
simplify :: Formula :: Formula
simplify f = case f of
    | NOT (NOT f1) -> simplify f1
    | OR (f1 (Const False)) -> simplify f1
    | OR (f1 (Const True)) -> simplify f1
-}

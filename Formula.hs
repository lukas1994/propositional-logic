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
    deriving (Show, Eq)

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

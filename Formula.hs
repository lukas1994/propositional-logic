module Formula
( Formula(..)
, Context
, eval
, simplify
, isCNF
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

simplify :: Formula -> Formula
simplify (NOT (Const b)) = Const (not b)
simplify f = f

isCNF :: Formula -> Bool
isCNF = isCNF' False
  where isCNF' :: Bool -> Formula -> Bool
        isCNF' _ (Var _) = True
        isCNF' _ (Const _) = True
        isCNF' _ (NOT (Var _)) = True
        isCNF' _ (NOT (Const _)) = True -- simplify before
        isCNF' _ (NOT _) = False
        isCNF' o (OR f1 f2) = (isCNF' True f1) && (isCNF' True f2)
        isCNF' o (AND f1 f2) = if o then False else (isCNF' o f1) && (isCNF' o f2)

main :: IO()
main = do
	let b = isCNF (OR (Var "P") (AND (Const True) (Var "W")))
	putStr . show $ b


module Language.Lambda 
( Lambda(..),
  Var(..),
  betaReduce,
  normal
) where

import Data.List (union, (\\))
import Data.Maybe
import Control.Monad

data Var = Var String deriving Eq

instance Show Var where
  show (Var x) = x

data Lambda = Term Var
            -- Application:
            | Lambda :$ Lambda
            -- Abstraction:
            | Var :-> Lambda

infixl 1 :$
infixr 2 :->

instance Show Lambda where
  show (Term v) = show v
  show (l1 :$ l2) = "(" ++ show l1 ++ " " ++ show l2 ++ ")"
  show (v :-> l) = "(λ" ++ show v ++ "." ++ show l ++ ")"

freshTermVar :: Lambda -> Var
freshTermVar l = head (vars \\ allTerms l)
  where vars                = map (\x -> Var [x]) ['a'..'z']
        allTerms (Term v)   = [v]
        allTerms (l1 :$ l2) = allTerms l1 `union` allTerms l2
        allTerms (v :-> l)  = allTerms l `union` [v]

freeVars :: Lambda -> [Var]
freeVars (Term v) = [v]
freeVars (v :-> l) = freeVars l \\ [v]
freeVars (l1 :$ l2) = freeVars l1 `union` freeVars l2

subs :: Var -> Lambda -> Lambda -> Lambda
subs x r (Term y)   = if x == y then r else Term y
subs x r (l1 :$ l2) = subs x r l1 :$ subs x r l2
subs x r (y :-> l)
  | x == y                 = y :-> l
  | x `notElem` freeVars l = y :-> l
  | y `notElem` freeVars r = y :-> subs x r l
  | otherwise              = z :-> subs x r (subs y (Term z) l)
    where z = freshTermVar (r :$ l)

betaReduce :: Lambda -> Maybe Lambda
betaReduce (Term v) = Nothing
betaReduce (x :-> l) = betaReduce l >>= (\r -> return (x :-> r))
betaReduce ((x :-> l1) :$ l2) = Just $ subs x l2 l1
betaReduce (l1 :$ l2) = (betaReduce l1 >>= (\r1 -> return (r1 :$ l2)))
                `mplus` (betaReduce l2 >>= (\r2 -> return (l1 :$ r2)))

normal :: Lambda -> Lambda
normal expr = case betaReduce expr of
  Just red -> normal red
  Nothing -> expr


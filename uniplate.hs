import Data.Generics.Uniplate.Direct

data Expr a
  = Fls
  | Tru
  | Var a
  | Not (Expr a)
  | And (Expr a) (Expr a)
  | Or  (Expr a) (Expr a)
  deriving (Show, Eq)

instance Uniplate (Expr a) where
  uniplate (Not f)     = plate Not |* f
  uniplate (And f1 f2) = plate And |* f1 |* f2
  uniplate (Or f1 f2)  = plate Or |* f1 |* f2
  uniplate x           = plate x

simplify :: Expr a -> Expr a
simplify = transform simp
 where
   simp (Not (Not f)) = f
   simp (Not Fls) = Tru
   simp (Not Tru) = Fls
   simp x = x

reduce :: Show a => Expr a -> Expr a
reduce = rewrite cnf
  where
    -- double negation
    cnf (Not (Not p)) = Just p

    -- de Morgan
    cnf (Not (p `Or` q))  = Just $ (Not p) `And` (Not q)
    cnf (Not (p `And` q)) = Just $ (Not p) `Or` (Not q)

    -- distribute conjunctions
    cnf (p `Or` (q `And` r)) = Just $ (p `Or` q) `And` (p `Or` r)
    cnf ((p `And` q) `Or` r) = Just $ (p `Or` q) `And` (p `Or` r)
    cnf _ = Nothing


example1 :: Expr String
example1 = simplify (Not (Not (Not (Not (Var "a")))))
-- Var "a"

example2 :: [String]
example2 = [a | Var a <- universe ex]
  where
    ex = Or (And (Var "a") (Var "b")) (Not (And (Var "c") (Var "d")))
-- ["a","b","c","d"]

example3 :: Expr String
example3 = reduce $ ((a `And` b) `Or` (c `And` d)) `Or` e
  where
    a = Var "a"
    b = Var "b"
    c = Var "c"
    d = Var "d"
    e = Var "e"

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- ∃ t. (t, t → t, t → String)
data Box = forall a. Box a (a -> a) (a -> String)

boxa :: Box
boxa = Box 1 negate show

boxb :: Box
boxb = Box "foo" reverse show

apply :: Box -> String
apply (Box x f p) = p (f x)

-- ∃ t. Show t => t
data SBox = forall a. Show a => SBox a

boxes :: [SBox]
boxes = [SBox (), SBox 2, SBox "foo"]

showBox :: SBox -> String
showBox (SBox a) = show a

main :: IO ()
main = mapM_ (putStrLn . showBox) boxes
-- ()
-- 2
-- "foo"

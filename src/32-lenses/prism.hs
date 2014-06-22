import Control.Lens

data Value = I Int
           | D Double
           deriving Show

_I :: Prism' Value Int
_I = prism remit review
  where
    remit :: Int -> Value
    remit a = I a

    review :: Value -> Either Value Int
    review (I a) = Right a
    review a     = Left a

_D :: Prism' Value Double
_D = prism remit review
  where
    remit :: Double -> Value
    remit a = D a

    review :: Value -> Either Value Double
    review (D a) = Right a
    review a     = Left a


test1 :: Maybe Int
test1 = (I 42) ^? _I

test2 :: Value
test2 = 42 ^. re _I

test3 :: Value
test3 = over _I succ (I 2)

test4 :: Value
test4 = over _I succ (D 2.71)

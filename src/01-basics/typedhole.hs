{-# OPTIONS -XPartialTypeSignatures #-}

head' = head _

const' :: _
const' x y = x

foo :: _a -> _a
foo _ = False

succ' :: _ => a -> a
succ' x = x + 1

main = undefined

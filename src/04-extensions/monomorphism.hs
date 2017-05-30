{-# LANGUAGE NoMonomorphismRestriction #-}

module Monomorphism (foo,bar) where

-- + extension: Num a => a -> a -> a
-- - extension: Num a => a -> a -> a
foo x y = x + y

-- + extension: Num a => a -> a
-- - extension: Integer -> Integer 
bar = foo 1


-- Now if this module is loaded without the extension,
-- then the call `bar 1.0` fails, since 1.0 is not a valid
-- Integer. If, however, `bar 1.0` was called somewhere within
-- this module, then there would be enough information to
-- correctly infer the type.

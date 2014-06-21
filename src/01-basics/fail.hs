import GHC.Base

foo :: a
foo = undefined
-- *** Exception: Prelude.undefined

bar :: a
bar = assert False undefined
-- *** Exception: src/fail.hs:8:7-12: Assertion failed

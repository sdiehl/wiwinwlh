import Data.Scientific

c, h, g, a, k :: Scientific
c = scientific 299792458 (0)   -- Speed of light
h = scientific 662606957 (-42) -- Planck's constant
g = scientific 667384    (-16) -- Gravitational constant
a = scientific 729735257 (-11) -- Fine structure constant
k = scientific 268545200 (-9)  -- Khinchin Constant

tau :: Scientific
tau = fromFloatDigits (2*pi)

maxDouble64 :: Double
maxDouble64 = read "1.7976931348623159e308"
-- Infinity

maxScientific :: Scientific
maxScientific = read "1.7976931348623159e308"
-- 1.7976931348623159e308

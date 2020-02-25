module Example where

import Data.Curve.Edwards.Ed25519 as Ed25519
import Protolude

-- generate random affine point
p1 :: Ed25519.PA
p1 = Ed25519.gen

-- generate affine point by multiply by field coefficient
p2 :: Ed25519.PA
p2 = Ed25519.mul p1 (3 :: Ed25519.Fr)

-- point addition
p3 :: Ed25519.PA
p3 = Ed25519.add p1 p2

-- point identity
p4 :: Ed25519.PA
p4 = Ed25519.id

-- point doubling
p5 :: Ed25519.PA
p5 = Ed25519.dbl p1

-- point inversion
p6 :: Ed25519.PA
p6 = Ed25519.inv p1

-- Frobenius endomorphism
p7 :: Ed25519.PA
p7 = Ed25519.frob p1

-- base point
p8 :: Ed25519.PA
p8 = Ed25519.gA

-- convert affine coordinates to projective coordinates
p9 :: Ed25519.PP
p9 = Ed25519.fromA p8

-- get y coordinate (point from Fq) from coordinate
p10 :: Maybe Ed25519.Fq
p10 = Ed25519.yX p1 (2 :: Fq)

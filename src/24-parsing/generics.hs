{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Parsec
import Text.Parsec.Text.Lazy
import Control.Applicative ((<*), (<*>), (<$>))
import GHC.Generics

class GParse f where
  gParse :: Parser (f a)

-- Type synonym metadata for constructors
instance (GParse f, Constructor c) => GParse (C1 c f) where
  gParse =
    let con = conName (undefined :: t c f a) in
    (fmap M1 gParse) <* string con

-- Constructor names
instance GParse f => GParse (D1 c f) where
  gParse = fmap M1 gParse

-- Sum types
instance (GParse a, GParse b) => GParse (a :+: b) where
  gParse = try (fmap L1 gParse <|> fmap R1 gParse)

-- Product types
instance (GParse f, GParse g) => GParse (f :*: g) where
  gParse = (:*:) <$> gParse <*> gParse

-- Nullary constructors
instance GParse U1 where
  gParse = return U1

data Scientist
  = Newton
  | Einstein
  | Schrodinger
  | Feynman
  deriving (Show, Generic)

data Musician
  = Vivaldi
  | Bach
  | Mozart
  | Beethoven
  deriving (Show, Generic)

gparse :: (Generic g, GParse (Rep g)) => Parser g
gparse = fmap to gParse

scientist :: Parser Scientist
scientist = gparse

musician :: Parser Musician
musician = gparse

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Singletons
import Data.Singletons.TH

$( singletons
     [d|
       data Nat = Zero | Succ Nat
         deriving (Eq, Show)

       plus :: Nat -> Nat -> Nat
       plus Zero n = n
       plus (Succ m) n = Succ (plus m n)

       isEven :: Nat -> Bool
       isEven Zero = True
       isEven (Succ Zero) = False
       isEven (Succ (Succ n)) = isEven n
       |]
 )

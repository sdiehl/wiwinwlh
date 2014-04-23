{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll #-}

import Control.Monad
import Control.Category
import Prelude hiding ((.))

-- Kleisli category
newtype Kleisli m a b = K (a -> m b)

-- Kleisli morphisms ( a -> m b )
type (a :~> b) m = Kleisli m a b

instance Monad m => Category (Kleisli m) where
  id            = K return
  (K f) . (K g) = K (f <=< g)


just :: (a :~> a) Maybe
just = K Just

left :: forall a b. (a :~> b) Maybe -> (a :~> b) Maybe
left f = just . f

right :: forall a b. (a :~> b) Maybe -> (a :~> b) Maybe
right f = f . just

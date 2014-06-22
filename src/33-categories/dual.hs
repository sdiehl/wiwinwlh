import Control.Category
import Prelude hiding ((.), id)

newtype Op a b = Op (b -> a)

instance Category Op where
  id = Op id
  (Op f) . (Op g) = Op (g . f)

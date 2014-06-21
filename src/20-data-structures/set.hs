import qualified Data.Set as Set

set :: Set.Set Integer
set = Set.fromList [1..1000]

memtest :: Integer -> Bool
memtest elt = Set.member elt set

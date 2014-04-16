import Data.Sequence

a :: Seq Int
a = fromList [1,2,3]

a0 :: Seq Int
a0 = a |> 4
-- [1,2,3,4]

a1 :: Seq Int
a1 = 0 <| a
-- [0,1,2,3]

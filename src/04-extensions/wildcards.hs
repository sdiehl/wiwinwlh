{-# LANGUAGE RecordWildCards #-}

data T = T { a :: Int , b :: Int }

f :: T -> Int
f (T {..} ) = a + b

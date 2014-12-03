ignore :: a -> Int
ignore x = 0

loop :: a
loop = loop

main :: IO ()
main = print $ ignore loop

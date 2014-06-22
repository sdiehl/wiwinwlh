{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Class

data PlatonicSolid
  = Tetrahedron
  | Cube
  | Octahedron
  | Dodecahedron
  | Icosahedron

pretty ''PlatonicSolid

main :: IO ()
main = do
  putStrLn (ppr Octahedron)
  putStrLn (ppr Dodecahedron)

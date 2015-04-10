import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sierpinski :: Int -> Diagram SVG R2
sierpinski 1 = eqTriangle 1
sierpinski n =
      s
     ===
  (s ||| s) # centerX
  where
    s = sierpinski (n - 1)

example :: Diagram SVG R2
example = sierpinski 5 # fc black

main :: IO ()
main = defaultMain example

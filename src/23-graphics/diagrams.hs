import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sierpinksi :: Int -> Diagram SVG R2
sierpinksi 1 = eqTriangle 1
sierpinksi n =
      s
     ===
  (s ||| s) # centerX
  where
    s = sierpinksi (n - 1)

example :: Diagram SVG R2
example = sierpinksi 5 # fc black

main :: IO ()
main = defaultMain example

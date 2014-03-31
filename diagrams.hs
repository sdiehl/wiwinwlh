import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram SVG R2
example = s ||| s
  where
    s = fc black $ eqTriangle 5

main :: IO ()
main = defaultMain example

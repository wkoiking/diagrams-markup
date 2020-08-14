module DiagramsMarkup.Type where

-- diagrams
import Diagrams.Prelude hiding (view)
-- diagrams-svg
import Diagrams.Backend.SVG (B)

type NormalDiagram = Diagram B

type FrexNormalDiagram = Double -> NormalDiagram

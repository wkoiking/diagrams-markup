module DiagramsMarkup where

-- diagrams
import Diagrams.Prelude ((|||))
import qualified Diagrams.Prelude as D
-- SVGFonts
import Graphics.SVGFonts (textSVG_, TextOpts(..))
-- diagrams-markup
import DiagramsMarkup.Type
-- safe
import Safe (lastMay)
-- base
import Data.List (find, inits)

twoColums
    :: Double -- ^ gap
    -> (Double -> [NormalDiagram]) -- ^ function to produce lines
    -> FrexNormalDiagram
twoColums gap toLines w = case find ((h' <) . totalHeight) $ inits ls of
    Just ls' -> D.hcat $ map (D.vsep gap) [ls', drop (length ls') ls]
    Nothing  -> mempty
 where h = totalHeight ls
       totalHeight = sum . map D.height
       ls = toLines w
       h' = h / 2

hcat
    :: [FrexNormalDiagram]
    -> FrexNormalDiagram
hcat = hsep 0

hsep
    :: Double -- ^ gap
    -> [FrexNormalDiagram]
    -> FrexNormalDiagram
hsep gap fs w = D.hsep gap $ map ($ w') fs
 where w' = (w - fromIntegral (n - 1) * gap) / fromIntegral n
       n = length fs
vcat
    :: [FrexNormalDiagram]
    -> FrexNormalDiagram
vcat = vsep 0

vsep
    :: Double -- ^ gap
    -> [FrexNormalDiagram]
    -> FrexNormalDiagram
vsep gap fs w = D.vsep gap $ map ($ w) fs

hcat2
    :: Double -- ^ minimum width of one column
    -> Bool -- ^ willWrap?
    -> [FrexNormalDiagram] -- ^ function to produce lines
    -> FrexNormalDiagram
hcat2 = undefined

viewPort
    :: (Double, Double) -- ^ width and height of view port
    -> (Double, Double) -- ^ view port pos
    -> NormalDiagram -- ^ diagram to be clipped
    -> NormalDiagram
viewPort = undefined

toLine
    :: (NormalDiagram -> NormalDiagram)
    -> NormalDiagram -- ^ any diagram
    -> FrexNormalDiagram
toLine align line w = D.alignL $ mconcat $ map align
    [ line
    , D.phantom (D.rect w 1 :: NormalDiagram)
    ]

dlines
    :: (NormalDiagram -> NormalDiagram) -- ^ Left or Center or Right
    -> Double -- ^ size
    -> TextOpts Double
    -> String
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
dlines align size opt str w
    = map (\ line -> toLine align line w)
    $ dlines' (dwords size opt str) w

dlines'
    :: [NormalDiagram] -- ^ words
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
dlines' [] _ = []
dlines' (d:ds) w = case lastMay $ takeWhile ((< w) . D.width . D.hsep 0.3) $ inits (d:ds) of
    Just []  -> d : dlines' ds w
    Just ds' -> D.hsep 0.3 ds' : dlines' (drop (length ds') (d:ds)) w
    _        -> [D.hsep 0.3 $ d:ds]

dwords
    :: Double -- ^ size
    -> TextOpts Double
    -> String -- ^ chars
    -> [NormalDiagram] -- ^ words
dwords size opt str = map (D.scale size . textSVG_ opt) $ words str

makeLinesWithFloat
    :: (NormalDiagram -> NormalDiagram) -- ^ Left or Right
    -> NormalDiagram -- ^ diagram to be floated
    -> [NormalDiagram] -- ^ words
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
makeLinesWithFloat = undefined

itemize
    :: NormalDiagram
    -> Double -- ^ size
    -> TextOpts Double
    -> [String]
    -> Double
    -> [[NormalDiagram]] -- ^ list of lines
itemize marker size opt strs w = map item strs
 where item :: String -> [NormalDiagram] -- ^ lines
       item str = case dlines D.alignL size opt str w' of
           [] -> []
           (l:ls) -> D.alignL (marker ||| l) : map (D.alignL . (D.phantom marker |||)) ls
        where w' = w - markerWidth
              markerWidth = D.width marker 


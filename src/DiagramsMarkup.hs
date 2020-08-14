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
    -> FrexDiagram
twoColums gap toLines w = case find ((h' <) . totalHeight) $ inits ls of
    Just ls' -> D.hcat $ map (D.vsep gap) [ls', drop (length ls') ls]
    Nothing  -> mempty
 where h = totalHeight ls
       totalHeight = sum . map D.height
       ls = toLines w
       h' = h / 2

hcat
    :: [FrexDiagram]
    -> FrexDiagram
hcat = hsep 0

hsep
    :: Double -- ^ gap
    -> [FrexDiagram]
    -> FrexDiagram
hsep gap fs w = D.hsep gap $ map ($ w') fs
 where w' = (w - fromIntegral (n - 1) * gap) / fromIntegral n
       n = length fs
vcat
    :: [FrexDiagram]
    -> FrexDiagram
vcat = vsep 0

vsep
    :: Double -- ^ gap
    -> [FrexDiagram]
    -> FrexDiagram
vsep gap fs w = D.vsep gap $ map ($ w) fs

hcat2
    :: Double -- ^ minimum width of one column
    -> Bool -- ^ willWrap?
    -> [FrexDiagram] -- ^ function to produce lines
    -> FrexDiagram
hcat2 = undefined

viewPort
    :: (Double, Double) -- ^ width and height of view port
    -> (Double, Double) -- ^ view port pos
    -> NormalDiagram -- ^ diagram to be clipped
    -> NormalDiagram
viewPort = undefined

dline
    :: (NormalDiagram -> NormalDiagram)
    -> NormalDiagram -- ^ any diagram
    -> FrexDiagram
dline align line w = D.alignL $ mconcat $ map align
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
dlines align size opt str w = dlines' align size (dwords opt str) w

dlines'
    :: (NormalDiagram -> NormalDiagram) -- ^ Left or Center or Right
    -> Double -- ^ size
    -> [NormalDiagram] -- ^ words
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
dlines' align size ws w = map (\ line -> dline align line w) $ go $ map (D.scale size) ws
 where go :: [NormalDiagram] -> [NormalDiagram]
       go (d:ds) = case lastMay $ takeWhile ((< w) . D.width . catWords) $ inits (d:ds) of
           Just []  -> d : go ds
           Just ds' -> catWords ds' : go (drop (length ds') (d:ds))
           _        -> [catWords $ d:ds]
       go [] = []
       catWords = D.hsep (size * 0.3)

dwords
    :: TextOpts Double
    -> String -- ^ string of words
    -> [NormalDiagram] -- ^ words
dwords opt str = map (dword opt) $ words str

dword
    :: TextOpts Double
    -> String -- ^ string of a word
    -> NormalDiagram
dword = textSVG_

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
    -> [String] -- ^ strings to be itemized
    -> Double
    -> [[NormalDiagram]] -- ^ list of lines
itemize marker size opt strs w = itemize' marker size (map (dwords opt) strs) w

itemize'
    :: NormalDiagram
    -> Double -- ^ size
    -> [[NormalDiagram]] -- ^ strings to be itemized
    -> Double
    -> [[NormalDiagram]] -- ^ list of lines
itemize' marker size strs w = map item strs
 where item
           :: [NormalDiagram] -- ^ words
           -> [NormalDiagram] -- ^ lines
       item ws = case dlines' D.alignL size ws w' of
           [] -> []
           (l:ls) -> D.alignL (marker ||| l) : map (D.alignL . (D.phantom marker |||)) ls
        where w' = w - markerWidth
              markerWidth = D.width marker 

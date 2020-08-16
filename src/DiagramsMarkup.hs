module DiagramsMarkup where

-- diagrams
import Diagrams.Prelude ((|||))
import qualified Diagrams.TwoD  as D
import qualified Diagrams.Prelude as D
-- SVGFonts
import Graphics.SVGFonts (textSVG_, TextOpts(..), Spacing(..), Mode(..))
import Graphics.SVGFonts.Fonts (loadDataFont, lin, bit)
import Graphics.SVGFonts.ReadFont (PreparedFont)
-- diagrams-markup
import DiagramsMarkup.Type
-- safe
import Safe (lastMay)
-- base
import Data.List (find, inits, unwords)
import System.IO.Unsafe (unsafePerformIO)

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

hcatBlock
    :: [FrexDiagram]
    -> FrexDiagram
hcatBlock = hsepBlock 0

hsepBlock
    :: Double -- ^ gap
    -> [FrexDiagram]
    -> FrexDiagram
hsepBlock gap fs w = D.hsep gap $ map ($ w') fs
 where w' = (w - fromIntegral (n - 1) * gap) / fromIntegral n
       n = length fs
vcatBlock
    :: [FrexDiagram]
    -> FrexDiagram
vcatBlock = vsepBlock 0

vsepBlock
    :: Double -- ^ gap
    -> [FrexDiagram]
    -> FrexDiagram
vsepBlock gap fs w = D.vsep gap $ map ($ w) fs

hcatBlock2
    :: Double -- ^ minimum width of one column
    -> Bool -- ^ willWrap?
    -> [FrexDiagram] -- ^ function to produce lines
    -> FrexDiagram
hcatBlock2 = undefined

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
dline align line lw = D.alignL $ mconcat $ map align
    [ line
    , D.phantom (D.rect lw 1 :: NormalDiagram)
    ]

dlines
    :: Font
    -> (NormalDiagram -> NormalDiagram) -- ^ Left or Center or Right
    -> Double -- ^ size
    -> String
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
dlines font align size str lw = map (alignLine . D.scale size . dword font) $ go $ words str
 where alignLine :: NormalDiagram -> NormalDiagram
       alignLine line = dline align line lw
       go :: [String] -> [String]
       go (w:ws) = case lastMay $ takeWhile ((< lw) . (* size) . stringWidth font . unwords) $ inits (w:ws) of
           Just []  -> w : go ws
           Just ws' -> unwords ws' : go (drop (length ws') (w:ws))
           _        -> [unwords $ w:ws]
       go [] = []

dlines'
    :: Double  -- ^ gap
    -> (NormalDiagram -> NormalDiagram) -- ^ Left or Center or Right
    -> Double -- ^ size
    -> [NormalDiagram] -- ^ words
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
dlines' gap align size ws = hsepInline (gap * size) align $  map (D.scale size) ws

hcatInline
    :: (NormalDiagram -> NormalDiagram) -- ^ Left or Center or Right
    -> [NormalDiagram] -- ^ inlineElements
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
hcatInline = hsepInline 0

hsepInline
    :: Double  -- ^ gap
    -> (NormalDiagram -> NormalDiagram) -- ^ Left or Center or Right
    -> [NormalDiagram] -- ^ inlineElements
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
hsepInline gap align elements lw = map (\ line -> dline align line lw) $ go elements
 where go :: [NormalDiagram] -> [NormalDiagram]
       go (d:ds) = case lastMay $ takeWhile ((< lw) . D.width . catWords) $ inits (d:ds) of
           Just []  -> d : go ds
           Just ds' -> catWords ds' : go (drop (length ds') (d:ds))
           _        -> [catWords $ d:ds]
       go [] = []
       catWords = D.hsep gap

dwords
    :: Font
    -> String -- ^ string of words
    -> [NormalDiagram] -- ^ words
dwords opt str = map (dword opt) $ words str

dword
    :: Font
    -> String -- ^ a word
    -> NormalDiagram
dword font str = mconcat
    [ D.scale 0.747765 $ D.font (fontName font) $ D.baselineText str
    , D.phantom $ (D.alignB $ D.alignL $ D.rect (stringWidth font str) 1 :: NormalDiagram)
    ]

stringWidth
    :: Font
    -> String
    -> Double
stringWidth (Font{..}) str = D.width (textSVG_ opt str :: NormalDiagram)
 where opt = TextOpts fontData INSIDE_H KERN False 1 1

makeLinesWithFloat
    :: (NormalDiagram -> NormalDiagram) -- ^ Left or Right
    -> NormalDiagram -- ^ diagram to be floated
    -> [NormalDiagram] -- ^ words
    -> Double -- ^ container width
    -> [NormalDiagram] -- ^ lines
makeLinesWithFloat = undefined

itemize
    :: Font
    -> NormalDiagram
    -> Double -- ^ size
    -> [String] -- ^ strings to be itemized
    -> Double
    -> [[NormalDiagram]] -- ^ list of lines
itemize font marker size strs lw = map item strs
 where item
           :: String -- ^ str
           -> [NormalDiagram] -- ^ lines
       item str = case dlines font D.alignL size str lw' of
           [] -> []
           (l:ls) -> D.alignL (marker ||| l) : map (D.alignL . (D.phantom marker |||)) ls
        where lw' = lw - markerWidth
              markerWidth = D.width marker 

itemize'
    :: Double -- ^ gap
    -> NormalDiagram
    -> Double -- ^ size
    -> [[NormalDiagram]] -- ^ strings to be itemized
    -> Double
    -> [[NormalDiagram]] -- ^ list of lines
itemize' gap marker size strs lw = map item strs
 where item
           :: [NormalDiagram] -- ^ words
           -> [NormalDiagram] -- ^ lines
       item ws = case dlines' gap D.alignL size ws lw' of
           [] -> []
           (l:ls) -> D.alignL (marker ||| l) : map (D.alignL . (D.phantom marker |||)) ls
        where lw' = lw - markerWidth
              markerWidth = D.width marker 

data Font = Font
    { fontName :: String
    , fontData :: PreparedFont Double
    , fontSpacing :: Double
    }

fontCalibri :: Font
fontCalibri = makeFont "Calibri" calibri

fontLin :: Font
fontLin = makeFont "LIbertine" lin

fontBit :: Font
fontBit = makeFont "Bitstream" bit

calibri :: IO (PreparedFont Double)
calibri = loadDataFont $ "fonts/Calibri.svg"

makeFont :: String -> IO (PreparedFont Double) -> Font
makeFont str font = Font{..}
 where fontName = str
       fontData = unsafePerformIO font 
       fontSpacing = D.width (textSVG_ option " " :: NormalDiagram)
       option = TextOpts fontData INSIDE_H KERN False 1 1

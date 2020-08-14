﻿{-# LANGUAGE OverloadedStrings #-}

module Main where

-- diagrams
import Diagrams.Prelude hiding (view)
-- diagrams-svg
import Diagrams.Backend.SVG (renderSVG)
-- diagrams-markup
import DiagramsMarkup.Type
import qualified DiagramsMarkup as M

main :: IO ()
main = renderTest

renderTest :: IO ()
renderTest = renderSVG  "topPage.svg" (mkWidth 5000) $ bg white $ topPage 50

topPage :: FrexNormalDiagram
topPage = M.vsep 0.5
    [ h1 "This is H1 header!"
    , M.hsep 3
        [ M.vsep 0.5
            [ h2 "This is H2 header"
            , p $ unlines
                [ "Hypertext Markup Language (HTML) is the standard markup language for "
                , "documents designed to be displayed in a web browser. It can be assisted "
                , "by technologies such as Cascading Style Sheets (CSS) and scripting "
                , "languages such as JavaScript."
                ]
            , ul
                [ "XHTML 1.0 was published as a W3C Recommendation on January 26, 2000,[57] and was later revised and republished on August 1, 2002. It offers the same three variations as HTML 4.0 and 4.01, reformulated in XML, with minor restrictions."
                , "XHTML 1.1[58] was published as a W3C Recommendation on May 31, 2001. It is based on XHTML 1.0 Strict, but includes minor changes, can be customized, and is reformulated using modules in the W3C recommendation 'Modularization of XHTML', which was published on April 10, 2001.[59]"
                , "and so on .."
                ]
            ]
        , M.vsep 0.5
            [ h2 "This is H2 header"
            , p $ unlines
                [ "Web browsers receive HTML documents from a web server or from local "
                , "storage and render the documents into multimedia web pages. HTML "
                , "describes the structure of a web page semantically and originally "
                , "included cues for the appearance of the document."
                ]
            , figure $ mconcat [circle 2 # fc red, triangle 10 # fc blue]
            , caption "This is circle on triangle" 
            ]
        ]
    , M.vsep 0.5
        [ h2 "This is H2 header"
        , p $ unlines
            [ "HTML elements are the building blocks of HTML pages. With HTML "
            , "constructs, images and other objects such as interactive forms may be "
            , "embedded into the rendered page. HTML provides a means to create "
            , "structured documents by denoting structural semantics for text such as "
            , "headings, paragraphs, lists, links, quotes and other items. HTML "
            , "elements are delineated by tags, written using angle brackets. Tags such "
            , "as <img /> and <input /> directly introduce content into the page. Other "
            , "tags such as <p> surround and provide information about document text "
            , "and may include other tags as sub-elements. Browsers do not display the "
            , "HTML tags, but use them to interpret the content of the page."
            ]
        ]
    ]
 where h1 :: String -> FrexNormalDiagram
       h1 str = blueStyle . vsep 0.5 . M.dlines alignL 2 def str
       h2 :: String -> FrexNormalDiagram
       h2 str = blueStyle . vsep 0.5 . M.dlines alignL 1.5 def str
       p :: String -> FrexNormalDiagram
       p str = blackStyle . vcat . M.dlines alignL 1 def str
       blueStyle = lw none . fc blue
       blackStyle = lw none . fc black
       figure :: NormalDiagram -> FrexNormalDiagram
       figure d = M.toLine center $ frame 1 d
       caption :: String -> FrexNormalDiagram
       caption str = blackStyle . vcat . M.dlines center 1 def str
       ul :: [String] -> FrexNormalDiagram
       ul strs = blackStyle . vsep 0.5 . map vcat . M.itemize squareBullet 1 def strs
        where squareBullet = translateY 0.2 $ mconcat [translateX 0.2 $ strutX 2, square 0.3]
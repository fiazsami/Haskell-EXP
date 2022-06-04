import Data.Sequence (Seq (Empty))

{----- Defining Data Types -----}

-- Style is composed of the style type & style params
data Style = EmptyStyle | Style String String
  deriving (Show)

-- Tag is composed of a tag with a list of styles
data Tag = Tag String [Style]
  deriving (Show)

-- Element is composed of a tag, and a list of elements, or it is purely content as as string
data Element = Element Tag [Element] | Content String
  deriving (Show)

{----- Defining Style examples -----}

-- Chaining a list of styles into one big style text
stylesStr :: [Style] -> String
stylesStr ((Style t p) : ss) = t ++ ":" ++ p ++ "; " ++ stylesStr ss
stylesStr _ = ""

-- Wrapping the style text into the correct format
completeStyle :: String -> String
completeStyle "" = ""
completeStyle s = " styles='" ++ s ++ "' "

-- Helper functions making the 3 examples styles
width :: String -> Style
width = Style "width"

height :: String -> Style
height = Style "height"

color :: String -> Style
color = Style "color"

{----- Defining the function to construct the Element text from a list of element -----}

elementStr :: [Element] -> String
-- If the element list is empty, then return empty string
elementStr [] = ""
-- Pattern matching the first element of the element list
elementStr (e : es) = case e of
  -- If the element is a content element, then simply return the content text
  (Content c) -> c
  -- If the element is a full element, then try to deconstruct the element
  (Element (Tag t styles@(s : ss)) childElem) ->
    -- Deconstructing like <openTag> ChildElem </closeTag>
    openTag t styles ++ elementStr childElem ++ closeTag t
      -- If there are multiple elements on parallel level, joint them together after the whole prior element is succesfully deconstructed
      ++ elementStr es
  -- Return empty string for any other cases, such as incomplete Tag declaration etc.
  _ -> ""
  where
    openTag t s = "<" ++ t ++ completeStyle (stylesStr s) ++ ">"
    closeTag t = "</" ++ t ++ ">"

{- Simple example of making the html code

    <html>
      <head styles='width:300px; height:100px; ' >Hello World!</head>
      <body styles='color:red; ' >
        <p>yo!</p>
        My name is Whatever!
      </body>
    </html>

-}

-- the html element with tag "html", no styles, `htmlElem` wrapped inside
html :: Element
html = Element (Tag "html" [EmptyStyle]) htmlElem

-- the head and body elements wrapped inside the html element
htmlElem :: [Element]
htmlElem =
  -- the head element: "head" tag, with `headStyles` as styles & "Hello World!" as content
  [ Element (Tag "head" headStyles) [Content "Hello World!"],
    -- the body element: "body" tag, with `bodyStyles` as styles
    -- 2 child elements wrapping inside, one <p>yo!</p> & one content " "My name is Whatever!"
    Element (Tag "body" bodyStyles) [Element (Tag "p" [EmptyStyle]) [Content "yo!"], Content "My name is Whatever!"]
  ]

-- The styles
headStyles :: [Style]
headStyles = [width "300px", height "100px"]

bodyStyles :: [Style]
bodyStyles = [color "red"]

-- Constructing the element
testElemStr :: String
testElemStr = elementStr [html]
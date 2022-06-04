{----- Defining Data Types -----}

-- Style is composed of the style type & style params
data Style = EmptyStyle | Style {styleType :: String, styleParam :: String}
  deriving (Show)

-- Tag is composed of a tag with a list of styles
data Tag = Tag {tag :: String, styles :: [Style]}
  deriving (Show)

-- Element is composed of a tag, and a list of elements, or it is purely content as as string
data Element = Element {elemTag :: Tag, childElement :: [Element]} | Content {content :: String}
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

-- >>> testElemStr
-- "<html><head styles='width:300px; height:100px; ' >Hello World!</head><body styles='color:red; ' ><p>yo!</p>My name is Whatever!</body></html>"

{----- Make the list of element a show instance -----}

newtype HTML = HTML {unHTML :: [Element]}

instance Show HTML where
  show (HTML e) = show $ elementStr e

testHTML :: HTML
testHTML = HTML [html]

-- >>> testHTML
-- "<html><head styles='width:300px; height:100px; ' >Hello World!</head><body styles='color:red; ' ><p>yo!</p>My name is Whatever!</body></html>"

{----- Now we can get the items back from the big element -----}

getHTMLElem :: HTML -> [Element]
getHTMLElem (HTML []) = error "No child exists"
getHTMLElem (HTML [e]) = childElement e

-- >>> getHTMLElem testHTML
-- [Element {elemTag = Tag {tag = "head", styles = [Style {styleType = "width", stylePram = "300px"},Style {styleType = "height", stylePram = "100px"}]}, childElement = [Content {content = "Hello World!"}]},Element {elemTag = Tag {tag = "body", styles = [Style {styleType = "color", stylePram = "red"}]}, childElement = [Element {elemTag = Tag {tag = "p", styles = [EmptyStyle]}, childElement = [Content {content = "yo!"}]},Content {content = "My name is Whatever!"}]}]

getNthChild :: Integer -> [Element] -> Element
getNthChild _ [] = error "Invalid length"
getNthChild n (e : es)
  | n == 1 = e
  | otherwise = getNthChild (n -1) es

-- >>> getNthChild 2 $ getHTMLElem testHTML
-- Element {elemTag = Tag {tag = "body", styles = [Style {styleType = "color", stylePram = "red"}]}, childElement = [Element {elemTag = Tag {tag = "p", styles = [EmptyStyle]}, childElement = [Content {content = "yo!"}]},Content {content = "My name is Whatever!"}]}

getStyles :: Element -> [Style]
getStyles = styles . elemTag

-- >>> getStyles $ getNthChild 2 $ getHTMLElem testHTML
-- [Style {styleType = "color", stylePram = "red"}]

getNthStyle :: Integer -> [Style] -> Style
getNthStyle _ [] = error "Invalid length"
getNthStyle n (s : ss)
  | n == 1 = s
  | otherwise = getNthStyle (n -1) ss

-- >>> getNthStyle 1 $ getStyles $ getNthChild 2 $ getHTMLElem testHTML
-- Style {styleType = "color", styleParam = "red"}

getStyleParam :: Style -> String
getStyleParam = styleParam

-- >>> getStyleParam $ getNthStyle 1 $ getStyles $ getNthChild 2 $ getHTMLElem testHTML
-- "red"

-- GOT IT!!!!
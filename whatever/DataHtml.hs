import Data.ByteString (foldl')
import GHC.Base (getTag)

{----- Defining Data Types -----}

-- Element is composed of a tag, and a list of elements, or it is purely content as as string
data Element = Element {elemTag :: Tag, childElement :: [Element]} | Content {content :: String}

instance Show Element where
  show e = show $ elementStr [e]

-- Tag is composed of a tag with a list of styles
data Tag = Tag {tag :: String, tagAttr :: [TagAttributes]}
  deriving (Show)

-- TagAttributes
data TagAttributes = EmptyAttr | TagStyle {tagStyle :: [Style]}
  deriving (Show)

-- Style is composed of the style type & style params
data Style = EmptyStyle | Style {styleType :: String, styleParam :: String}
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
elementStr [] = "" -- Return empty string for no element inputs
elementStr (e : es) = case e of -- Pattern match first element
  (Content c) -> c -- if its content then return the content string
  (Element (Tag t aa@(a : as)) childElem) ->
    -- if the element is a full element with deconstruction of input
    openTag t (allAttr aa) ++ elementStr childElem ++ closeTag t -- Construct the the basic element
      ++ elementStr es -- for the same level elements, append them after the previous is closed
    where
      openTag t s = "<" ++ t ++ s ++ ">"
      closeTag t = "</" ++ t ++ ">"

      -- construct the attribution text here
      allAttr [] = ""
      allAttr (a : as) = case a of -- pattern match the first attributes of the attr list
        (TagStyle s) -> completeStyle $ stylesStr s ++ allAttr as -- if it is a style attribute, construct like styles attribution then check the remaining attributes
        EmptyAttr -> allAttr as -- neglect the empty attribute
  _ -> "" -- return empty string for all other cases

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
html = Element (Tag "html" [EmptyAttr]) htmlElem

-- the head and body elements wrapped inside the html element
htmlElem :: [Element]
htmlElem =
  -- the head element: "head" tag, with `headStyles` as styles & "Hello World!" as content
  [ Element (Tag "head" [TagStyle headStyles]) [Content "Hello World!"],
    -- the body element: "body" tag, with `bodyStyles` as styles
    -- 2 child elements wrapping inside, one <p>yo!</p> & one content " "My name is Whatever!"
    Element (Tag "body" [TagStyle bodyStyles]) [Element (Tag "p" [EmptyAttr]) [Content "yo!"], Content "My name is Whatever!"]
  ]

-- The styles
headStyles :: [Style]
headStyles = [width "300px", height "100px"]

bodyStyles :: [Style]
bodyStyles = [color "red"]

-- Constructing the element
testElemStr :: String
testElemStr = elementStr [html]

-- >>> html
-- Element {elemTag = Tag {tag = "html", tagAttr = [EmptyAttr]}, childElement = [Element {elemTag = Tag {tag = "head", tagAttr = [TagStyle {tagStyle = [Style {styleType = "width", styleParam = "300px"},Style {styleType = "height", styleParam = "100px"}]}]}, childElement = [Content {content = "Hello World!"}]},Element {elemTag = Tag {tag = "body", tagAttr = [TagStyle {tagStyle = [Style {styleType = "color", styleParam = "red"}]}]}, childElement = [Element {elemTag = Tag {tag = "p", tagAttr = [EmptyAttr]}, childElement = [Content {content = "yo!"}]},Content {content = "My name is Whatever!"}]}]}

-- >>> testElemStr
-- "<html><head styles='width:300px; height:100px; ' >Hello World!</head><body styles='color:red; ' ><p>yo!</p>My name is Whatever!</body></html>"

{----- Make the list of element a show instance -----}

newtype HTML = HTML {unHTML :: Element}

instance Show HTML where
  show (HTML e) = show $ elementStr [e]

testHTML :: HTML
testHTML = HTML html

-- >>> testHTML
-- "<html><head styles='width:300px; height:100px; ' >Hello World!</head><body styles='color:red; ' ><p>yo!</p>My name is Whatever!</body></html>"

{----- Now we can get the items back from the big element - Example here: try to get the "red" from body -----}

-- Getting the list of element from HTML
-- >>> unHTML testHTML
-- Element {elemTag = Tag {tag = "html", tagAttr = [EmptyAttr]}, childElement = [Element {elemTag = Tag {tag = "head", tagAttr = [TagStyle {tagStyle = [Style {styleType = "width", styleParam = "300px"},Style {styleType = "height", styleParam = "100px"}]}]}, childElement = [Content {content = "Hello World!"}]},Element {elemTag = Tag {tag = "body", tagAttr = [TagStyle {tagStyle = [Style {styleType = "color", styleParam = "red"}]}]}, childElement = [Element {elemTag = Tag {tag = "p", tagAttr = [EmptyAttr]}, childElement = [Content {content = "yo!"}]},Content {content = "My name is Whatever!"}]}]}

getNthChild :: Integer -> [Element] -> Element
getNthChild _ [] = error "Invalid length"
getNthChild n (e : es)
  | n == 1 = e
  | otherwise = getNthChild (n -1) es

-- Get the child element of the html element
-- >>> childElement $ unHTML testHTML
-- [Element {elemTag = Tag {tag = "head", tagAttr = [TagStyle {tagStyle = [Style {styleType = "width", styleParam = "300px"},Style {styleType = "height", styleParam = "100px"}]}]}, childElement = [Content {content = "Hello World!"}]},Element {elemTag = Tag {tag = "body", tagAttr = [TagStyle {tagStyle = [Style {styleType = "color", styleParam = "red"}]}]}, childElement = [Element {elemTag = Tag {tag = "p", tagAttr = [EmptyAttr]}, childElement = [Content {content = "yo!"}]},Content {content = "My name is Whatever!"}]}]

-- Get the body (2nd) element
-- >>> getNthChild 2 $ childElement $ unHTML testHTML
-- Element {elemTag = Tag {tag = "body", tagAttr = [TagStyle {tagStyle = [Style {styleType = "color", styleParam = "red"}]}]}, childElement = [Element {elemTag = Tag {tag = "p", tagAttr = [EmptyAttr]}, childElement = [Content {content = "yo!"}]},Content {content = "My name is Whatever!"}]}

getStyleFromAttr :: [TagAttributes] -> [Style]
getStyleFromAttr [] = [EmptyStyle]
getStyleFromAttr (a : as) = case a of
  (TagStyle s) -> s
  _ -> getStyleFromAttr as

getStyles :: Element -> [Style]
getStyles = getStyleFromAttr . tagAttr . elemTag

-- Get the style from body
-- >>> getStyles $ getNthChild 2 $ childElement $ unHTML testHTML
-- [Style {styleType = "color", styleParam = "red"}]

getNthStyle :: Integer -> [Style] -> Style
getNthStyle _ [] = error "Invalid length"
getNthStyle n (s : ss)
  | n == 1 = s
  | otherwise = getNthStyle (n -1) ss

-- >>> getNthStyle 1 $ getStyles $ getNthChild 2 $ childElement $ unHTML testHTML
-- Style {styleType = "color", styleParam = "red"}

getStyleParam :: Style -> String
getStyleParam = styleParam

-- >>> getStyleParam $ getNthStyle 1 $ getStyles $ getNthChild 2 $ childElement $ unHTML testHTML
-- "red"

-- GOT IT!!!!

{----- Query Function -----}

getElement ::
  Element -> -- 1st argument: The large Element to query
  [Int] -> -- 2nd argument: Order of Penetration
  Element
getElement e [] = e
getElement e ls = foldr (\x acc -> childElement acc !! x) e (reverse ls)

-- >>> getElement html [1]
-- >>> getElement html [1,1]
-- "<body styles='color:red; ' ><p>yo!</p>My name is Whatever!</body>"
-- "My name is Whatever!"

getElemTag :: Element -> [Int] -> String
getElemTag e ls = tag $ elemTag $ getElement e ls

getData ::
  Element -> -- 1st argument: The large Element to query
  [Int] -> -- 2nd argument: Order of Penetration
  (Int, [Int]) -> -- 3rd argument: (Position of target attribute, attribute specific lookup logic)
  String
getData e ls (a, t) = case (tagAttr (elemTag $ getElement e ls) !! a, t) of
  (TagStyle ts, [0]) -> "styles"
  (TagStyle ts, [x, y]) -> case ts !! x of
    Style st sp -> checkStyle y st sp
      where
        checkStyle x st sp
          | x == 0 = st
          | x == 1 = sp
          | otherwise = "Lookup Fail (style lookup error)"
    _ -> "Lookup Fail (style lookup error)"
  _ -> "Lookup Fail"

-- e.g. want to obtain the "red"
-- >>> getData html [1] (0,[0,1])
-- "red"

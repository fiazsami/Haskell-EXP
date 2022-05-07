import System.IO ()

writeTo :: FilePath -> String -> IO ()
writeTo path html = do
    writeFile path html
toStr :: [[Char]] -> [Char]
toStr list = link list ""
    where
        link [] r = r
        link (x:xs) r = link xs (r ++ x)

styleAttr :: [Char] -> [Char]
styleAttr s = " style=\"" ++ s ++ "\""

openTag :: [Char] -> [[Char]] -> [Char]
openTag t s = "<" ++ t ++ styleAttr (toStr s) ++ ">"
closeTag :: [Char] -> [Char]
closeTag t = "</" ++ t ++ ">"

element :: [Char] -> [[Char]] -> [Char]
element t s = openTag t s ++ closeTag t
container :: [Char] -> [[Char]] -> [Char] -> [Char]
container t s c = openTag t s ++ c ++ closeTag t

style :: [Char] -> [Char] -> [Char]
style key val = key ++ ":" ++ val ++ ";"

bgColor :: [Char] -> [Char]
bgColor = style "background-color"
width :: [Char] -> [Char]
width = style "width"
height :: [Char] -> [Char]
height = style "height"
opacity :: [Char] -> [Char]
opacity = style "opacity"

inlineBlock :: [Char]
inlineBlock = style "display" "inline-block"

lineUp :: [Char] -> [Char]
lineUp = container "div" [inlineBlock]

rect :: [Char] -> [Char] -> [[Char]]
rect w h = [width w, height h]
square :: [Char] -> [[Char]]
square d = rect d d

coloredRect :: [Char] -> [Char] -> [Char] -> [[Char]]
coloredRect w h c = [width w, height h, c]

baseRect :: [Char] -> [[Char]]
baseRect = coloredRect "184px" "26px"
cell :: [Char] -> [Char]
cell color = element "div" (baseRect color)

fade :: Show a => [Char] -> a -> [Char]
fade color op = toStr [bgColor color, opacity (show op)]

fadeRed :: Double -> String
fadeRed = fade "red"
fadeOrange :: Double -> String
fadeOrange = fade "orange"
fadeYellow :: Double -> String
fadeYellow = fade "yellow"
fadeGreen :: Double -> String
fadeGreen = fade "green"
fadeBlue :: Double -> String
fadeBlue = fade "blue"
fadeIndigo :: Double -> String
fadeIndigo = fade "indigo"
fadeViolet :: Double -> String
fadeViolet = fade "violet"

fader :: (t -> Bool) -> (t -> p -> t) -> t -> p -> (t -> [Char]) -> [Char]
fader chk upd s d c = fdr s []
    where
        fdr l r = if chk l then lineUp r else fdr (upd l d) (cell (c l) ++ r)

fadeIn :: Double -> (Double -> [Char]) -> [Char]
fadeIn = fader (< 0) (-) 1
fadeOut :: Double -> (Double -> [Char]) -> [Char]
fadeOut = fader (> 1) (+) 0

interval :: Double
interval = 0.05

row1 :: [Char]
row1 = toStr [fadeIn interval fadeGreen, fadeOut interval fadeOrange, fadeIn interval fadeIndigo, fadeOut interval fadeYellow, fadeIn interval fadeViolet, fadeOut interval fadeRed, fadeIn interval fadeRed]

row2 :: [Char]
row2 = toStr [fadeOut interval fadeIndigo, fadeIn interval fadeYellow, fadeOut interval fadeViolet, fadeIn interval fadeBlue, fadeOut interval fadeGreen, fadeIn interval fadeOrange, fadeOut interval fadeBlue]


-- >>> writeTo "test.html" (row1 ++ row2)


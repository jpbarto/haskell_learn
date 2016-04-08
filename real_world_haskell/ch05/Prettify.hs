module Prettify
where

import Numeric (showHex)
import Data.Bits (shiftR, .&.)

data Doc = Empty |
            Char Char |
            Text String |
            Line    |
            Concat Doc Doc |
            Union Doc Doc |
        deriving (Show, Eq)

empty :: Doc
empty = Empty

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text "" = Empty
text t = Text t

double :: Double -> Doc
double d = text (show d)

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = Concat x y

char :: Char -> Doc
char c = Char c

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

oneChar :: Char -> Doc
oneChar c = let
        mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
    in
        case lookup c simpleEscapes of
            Just r -> text r
            Nothing | mustEscape c -> hexEscape c
                    | otherwise -> char c

simpleEscapes :: [(Char, String)]
simpleEscapes = let
        ch a b = (a, ['\\',b])
    in
        zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"

smallHex :: Int -> Doc
smallHex x = let
        h = showHex x ""
    in
        text "\\u" <>
        text (replicate (4 - length h) '0') <>
        text h

astral :: Int -> Doc
astral n = let
        a = (shiftR n 10) .&. 0x3ff
        b = n .&. 0x3ff
    in
        smallHex (a + 0xd800) <> smallHex (b + 0xdc00)

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = aastral (d - 0x10000)
    where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten Line = Char ' '
flatten (Union x _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

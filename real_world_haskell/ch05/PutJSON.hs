module PutJSON
where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = let
        pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v) = show k ++": "++ renderJValue v
    in
        "{"++ pairs o ++"}"

renderJValue (JArray a) = let
        values [] = ""
        values vs = intercalate ", " (map renderJValue vs)
    in
        "["++ values a ++"]"

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)


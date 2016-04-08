module PrettyJSON (
    renderJValue
)
where

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, puncuate, text, compact, pretty)

renderJValue :: JValue -> Doc

renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber n) = double n
renderJValue (JString s) = string s
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = let
        field (name, val) = string name <>
            text ": " <>
            renderJValue val
    in
        seriesl '{' '}' field obj

import SimpleJSON
import PutJSON

main = let
        obj = JObject [("a", JString "b"), ("c", JNumber 9), ("bar", JBool False)]
    in do
        print (obj)
        print (renderJValue obj)

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number 7!"
lucky x = "Sorry, no winner this time"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (a,b) (c,d) = (a + c, b + d)

head' :: [a] -> a
head' [] = error "Cannot remove an element from an empty list"
head' (x:_) = x

f :: Float -> Float
f x = x * x

g :: Double -> Double
g x = x * x

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normal"
    | bmi <= overweight = "Overweight"
    | otherwise = "Obese"
    where   
        bmi = w / h ^ 2
        skinny = 18.5
        normal = 25
        overweight = 30

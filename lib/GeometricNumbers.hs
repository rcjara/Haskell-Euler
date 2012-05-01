module GeometricNumbers
( triangle
, triangles
) where

triangle i = round $ (i + 1) * (i / 2.0)

triangles = map triangle [1..]

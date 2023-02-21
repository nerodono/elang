module ELang.Utils
( mapFst
, mapSnd
)
where

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (left, right) = (f left, right)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (left, right) = (left, f right)

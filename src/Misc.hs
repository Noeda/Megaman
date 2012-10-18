module Misc(anyValue) where

-- just like Prelude.any but returns the item as Maybe a
anyValue :: (a -> Maybe b) -> [a] -> Maybe b
anyValue _ [] = Nothing
anyValue predicate (i:rest) = case predicate i of
                                Nothing -> anyValue predicate rest
                                x       -> x


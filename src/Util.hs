module Util
  ( push
  )
  where

push :: a -> [a] -> [a]
push value list = list ++ [value]

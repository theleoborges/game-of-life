module Utils where

ap : (a -> b -> c)
      -> (a -> b)
      -> (a -> c)
ap f g = \a -> f a (g a)

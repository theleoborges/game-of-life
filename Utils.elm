module Utils where

ap : (a -> b -> c)
      -> (a -> b)
      -> (a -> c)
ap f g = \a -> f a (g a)


grid2d : Int -> Int -> List (Int, Int)
grid2d rows columns =
  [0..rows]
    |> List.concatMap
         (\y ->
            [0..columns]
              |> List.map
                 (\x -> (x, y)))

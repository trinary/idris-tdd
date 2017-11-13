
maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = x >>= \x_val =>
               y >>= \y_val => Just (x_val + y_val)


maybeAdd2 : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd2 x y = do
  xval <- x
  yval <- y
  Just (xval + yval)

maybeList = [[Just 4, Just 5, Just 10], [Just 3, Nothing], [Just 10]]

main = do
  print $ (fmap . fmap . fmap) (+1) maybeList

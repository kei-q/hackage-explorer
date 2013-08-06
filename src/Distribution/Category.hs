module Distribution.Category where

import Text.Trifecta
import Data.Monoid (mempty)

parse :: String -> [String]
parse src = case parseString categoriesParser mempty src of
    Success a -> a
    Failure _ -> ["Undefined"]

categoriesParser :: Parser [String]
categoriesParser = commaSep1 categoryIdentifier
    where categoryIdentifier = some (noneOf ",")

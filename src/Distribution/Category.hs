module Distribution.Category where

import Text.Trifecta
import Data.Monoid (mempty)

parse :: String -> [String]
parse source = case parseString categoriesParser mempty source of
    Success a -> a
    Failure e -> ["Undefined"]

categoriesParser = commaSep1 categoryIdentifier
    where categoryIdentifier = some (noneOf ",")

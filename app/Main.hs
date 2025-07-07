module Main (main) where

import Core (runParser)
import Hson (parseJsonValue)

main :: IO ()
main = do
    let parsed = runParser parseJsonValue "true"
    print parsed

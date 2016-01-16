module Main where

import Lib

main :: IO ()
main = do
  downloadSubredditJsonToFile "askreddit" "/Users/anthony/hot.json"
  convertSubredditToHtml "askreddit" "/Users/anthony/hot.html"

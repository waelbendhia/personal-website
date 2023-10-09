module Main where

import Application
import Options.Applicative
import Relude

main :: IO ()
main = runApp =<< execParser (info configParser fullDesc)

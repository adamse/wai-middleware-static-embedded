{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Network.Wai.Middleware.StaticEmbedded -- This package
import Data.FileEmbed -- file-embed
import Web.Scotty -- scotty

main :: IO ()
main = scotty 1337 $ do
  middleware (static $(embedDir ".")) -- serves the source directory
  notFound (text "404: Not found!")

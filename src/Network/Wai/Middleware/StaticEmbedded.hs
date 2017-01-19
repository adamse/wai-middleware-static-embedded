{-# LANGUAGE OverloadedStrings #-}
-- |

module Network.Wai.Middleware.StaticEmbedded (static) where

import           Crypto.Hash
import           Data.ByteArray.Encoding
import           Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import qualified Data.Text as T
import           Network.HTTP.Types (status200, status304)
import           Network.Mime (defaultMimeLookup)
import           Network.Wai

static :: [(FilePath, ByteString)] -> Middleware
static files app req callback =
  fromMaybe (app req callback) $ do
    let fileName = T.unpack . T.intercalate "/" $ pathInfo req
    file <- fmap BL.fromStrict $ lookup fileName files
    let mime = defaultMimeLookup (T.pack fileName)
    let etag = getEtag file
    return $
      if checkModified etag (readHeader "If-None-Match")
        then sendNotModified etag
        else sendFile file etag mime
  where
    readHeader header = lookup header $ requestHeaders req
    checkModified etag metag = maybe False (etag ==) metag
    computeHeaders etag =
      [ ("Cache-Control", "no-transform,public,max-age=300,s-maxage=900")
      , ("ETag", etag)
      , ("Vary", "Accept-Encoding")
      ]
    sendNotModified etag = do
      let headers = computeHeaders etag
      callback $ responseLBS status304 headers BL.empty
    sendFile file etag mime = do
      let headers = ("Content-Type", mime) : computeHeaders etag
      callback $ responseLBS status200 headers file

getEtag :: BL.ByteString -> B.ByteString
getEtag file = convertToBase Base16 (hashlazy file :: Digest SHA1)

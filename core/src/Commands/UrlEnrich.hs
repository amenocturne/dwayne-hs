{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands.UrlEnrich
  ( enrichText,
    detectUrls,
    buildRichText,
  )
where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl', nub)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Encoding as TE
import Model.OrgMode (RichText (..), TextNode (..), plainToRichText)
import Network.HTTP.Client
  ( httpLbs,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Process (readProcess)

-- | Check if a text fragment looks like an HTTP(S) URL
isHttpUrl :: T.Text -> Bool
isHttpUrl t = "http://" `T.isPrefixOf` t || "https://" `T.isPrefixOf` t

-- | Detect all unique HTTP(S) URLs in text by splitting on whitespace
detectUrls :: T.Text -> [T.Text]
detectUrls = nub . filter isHttpUrl . T.words

-- | Extract the <title> tag content from an HTML response body
extractTitle :: BSL.ByteString -> Maybe T.Text
extractTitle body =
  let text = TE.decodeUtf8With lenientDecode (BSL.toStrict body)
      lower = T.toLower text
   in case T.breakOn "<title" lower of
        (_, rest) | T.null rest -> Nothing
        (prefix, _) ->
          let offset = T.length prefix
              afterOpenTag = T.drop offset text
              afterClose = T.drop 1 $ snd $ T.breakOn ">" afterOpenTag
              titleContent = fst $ T.breakOn "<" afterClose
           in case T.strip titleContent of
                t | T.null t -> Nothing
                t -> Just t

-- | Check if a URL is a YouTube or YouTube Music link
isYouTubeUrl :: T.Text -> Bool
isYouTubeUrl url =
  any
    (`T.isInfixOf` url)
    [ "youtube.com/watch",
      "youtu.be/",
      "music.youtube.com/watch",
      "youtube.com/shorts/"
    ]

-- | Fetch title via yt-dlp for YouTube URLs (bypasses region restrictions)
fetchYouTubeTitle :: T.Text -> IO (Maybe T.Text)
fetchYouTubeTitle url = do
  result <- try $ do
    out <- readProcess "yt-dlp" ["--skip-download", "--print", "title", T.unpack url] ""
    return $ case T.strip (T.pack out) of
      t | T.null t -> Nothing
      t -> Just t
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right t -> return t

-- | Fetch the page title from a URL, returning Nothing on any failure
fetchTitle :: T.Text -> IO (Maybe T.Text)
fetchTitle url
  | isYouTubeUrl url = fetchYouTubeTitle url
  | otherwise = fetchTitleHttp url

-- | Fetch page title via HTTP for generic URLs
fetchTitleHttp :: T.Text -> IO (Maybe T.Text)
fetchTitleHttp url = do
  manager <- newManager tlsManagerSettings
  result <- try $ do
    req <- parseRequest (T.unpack url)
    let req' =
          req
            { requestHeaders = [("User-Agent", "Mozilla/5.0 (compatible; Dwayne/1.0)")],
              responseTimeout = responseTimeoutMicro 5000000
            }
    resp <- httpLbs req' manager
    return $ extractTitle (responseBody resp)
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right t -> return t

-- | Fetch titles for all given URLs
fetchAllTitles :: [T.Text] -> IO (M.Map T.Text (Maybe T.Text))
fetchAllTitles urls = do
  titles <- mapM (\url -> do t <- fetchTitle url; return (url, t)) urls
  return $ M.fromList titles

-- | Split a PlainText node by URL occurrences, replacing with OrgLink nodes
splitByUrl :: T.Text -> Maybe T.Text -> T.Text -> [TextNode]
splitByUrl url mTitle text =
  case T.breakOn url text of
    (_, match) | T.null match -> [PlainText text | not (T.null text)]
    (before, match) ->
      let after = T.drop (T.length url) match
          beforeNodes = [PlainText before | not (T.null before)]
          linkNode = OrgLink url mTitle
       in beforeNodes ++ [linkNode] ++ splitByUrl url mTitle after

-- | Replace all occurrences of a URL in RichText PlainText nodes with OrgLink nodes
replaceUrlInRichText :: T.Text -> Maybe T.Text -> RichText -> RichText
replaceUrlInRichText url mTitle (RichText nodes) = RichText $ concatMap processNode nodes
  where
    processNode (PlainText t) = splitByUrl url mTitle t
    processNode other = [other]

-- | Build RichText from plain text, replacing URLs with OrgLink nodes
buildRichText :: T.Text -> M.Map T.Text (Maybe T.Text) -> RichText
buildRichText text titles =
  let initial = RichText [PlainText text | not (T.null text)]
   in foldl' (\rt (url, mTitle) -> replaceUrlInRichText url mTitle rt) initial (M.toList titles)

-- | Enrich text by detecting URLs, fetching their page titles,
-- and building RichText with OrgLink nodes
enrichText :: T.Text -> IO RichText
enrichText text = do
  let urls = detectUrls text
  if null urls
    then return $ plainToRichText text
    else do
      titles <- fetchAllTitles urls
      return $ buildRichText text titles

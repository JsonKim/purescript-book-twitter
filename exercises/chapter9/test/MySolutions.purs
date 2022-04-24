module Test.MySolutions
  ( concatenateFiles
  , concatenateMany
  , concatenateManyParallel
  , countCharacters
  , countCharacters'
  , getWithTimeout
  , writeGet
  , recurseFiles
  )
  where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Control.Parallel (parOneOf, parTraverse)
import Data.Array (concat, fold, (:))
import Data.Either (Either, hush)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path
import Test.HTTP (getUrl)

-- Note to reader: Add your solutions to this file
concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles file1 file2 dest = do
  c1 <- readTextFile UTF8 file1
  c2 <- readTextFile UTF8 file2
  writeTextFile UTF8 dest $ c1 <> c2

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany files dest = do
  c <- traverse (readTextFile UTF8) files
  writeTextFile UTF8 dest $ fold c

countCharacters' :: FilePath -> Aff (Either Error Int)
countCharacters' file = do
  c <- attempt $ readTextFile UTF8 file
  pure $ length <$> c

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file = attempt do
  c <- readTextFile UTF8 file
  pure $ length c

writeGet :: String -> FilePath -> Aff Unit
writeGet url out = do
  str <- getUrl url
  writeTextFile UTF8 out str

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel files dest = do
  c <- parTraverse (readTextFile UTF8) files
  writeTextFile UTF8 dest $ fold c

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout ms url = do
  parOneOf
    [ AX.get AXRF.string url <#> hush <#> map _.body
    , delay (Milliseconds ms) $> Nothing
    ]

-- do block에서 let을 쓸때는 in을 생략해야 한다.
-- https://discourse.purescript.org/t/let-in-vs-let-which-should-i-use/2157
recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles file = do
  c <- readTextFile UTF8 file
  case c of
    "" -> pure [file]
    _ ->
      let
        dir = Path.dirname file

        files = split (Pattern "\n") c

        filesFromRoot = map (\f -> Path.concat [ dir, f ]) files
      in do
        arrarr <- parTraverse recurseFiles filesFromRoot
        pure $ file : concat arrarr

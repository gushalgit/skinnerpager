{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications #-}

module HCat (runHCat, paginate) where

-- R. Skinner, Kap. 8

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Info as SysInfo
import qualified System.IO.Error as IOError
import System.IO
import System.Process (readProcess)
import System.Environment (getArgs)

runHCat :: IO ()
runHCat = do
      filePath <- eitherToError =<< handleArgs
      content <- TextIO.hGetContents =<< openFile filePath ReadMode
      termSize <- getTerminalSize
      let pages = paginate termSize content
      showPages pages

-- Input handling

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        [] -> Left "Dateiname fehlt"
        _mehrereArgumente -> Left "es ist nur ein Dateiname erlaubt"

-- Terminal based IO

-- Behandlung von Text als Menge von Zeichen, die auf einer Seite der
-- Groesse ze x sp dargestellt werden sollen

data ScreenDimension = ScreenDimension
  { rows :: Int,
    cols :: Int
  }
  deriving (Show)

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  ch <- getChar
  case ch of
    ' ' -> return Continue
    'q' -> return Cancel
    _  -> getContinue
    
getTerminalSize :: IO ScreenDimension
getTerminalSize =
  case SysInfo.os of
    "darwin" -> tputScreenDimension
    "linux" -> tputScreenDimension
    _other -> pure $ ScreenDimension 25 80
  where
    tputScreenDimension :: IO ScreenDimension
    tputScreenDimension = do
      lin <- readProcess "tput" ["lines"] ""
      col <- readProcess "tput" ["cols"] ""
      let lines' = read $ init lin
          cols' = read $ init col
      return $ ScreenDimension (lines' - 1) cols'
      
clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

-- Textprocessing

paginate :: ScreenDimension -> Text.Text -> [Text.Text]
paginate sdim txt =
  let allLines = Text.lines txt
      wrappedLines = concatMap (softWrap (cols sdim)) allLines
      pageLines = groupsOf (rows sdim) wrappedLines
   in map Text.unlines pageLines

softWrap :: Int -> Text.Text -> [Text.Text]
softWrap maxlg line
  | Text.length line <= maxlg = [line]
  | otherwise =
      let (candidate, rest) = Text.splitAt maxlg line
          (firstPart, overflow) =
            if Text.head rest == ' '
              then (candidate, "")
              else splitOnSpaceAt (maxlg - 1) candidate
       in firstPart : softWrap maxlg (overflow <> rest)
  where
    splitOnSpaceAt textIdx cand
      | textIdx <= 0 = (cand, Text.empty)
      | Text.index cand textIdx == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIdx cand
           in (wrappedLine, Text.tail rest)
      | otherwise = splitOnSpaceAt (textIdx - 1) cand

-- Output

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) = do
  clearScreen
  TextIO.putStr page
  ch <- getContinue
  case ch of
    Continue -> showPages pages
    Cancel -> return ()

-- Utilities

eitherToError :: (Show a) => Either a b -> IO b
eitherToError (Right x) = return x
eitherToError (Left e) =
  Exception.throwIO . IOError.userError $ show e

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
   in hd : groupsOf n tl

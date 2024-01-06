{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- R. Skinner, Kap. 8

module HCat (runHCat, paginate) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import System.Environment (getArgs)
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Info as SysInfo
import System.Process (readProcess)
import Text.Printf (printf)

runHCat :: IO ()
runHCat = do
  filePath <- eitherToError =<< handleArgs
  content <- TextIO.hGetContents =<< openFile filePath ReadMode
  finfo <- fileInfo filePath
  termSize <- getTerminalSize
  let pages = paginate termSize finfo content
  hSetBuffering stdout NoBuffering   -- fuer die Ausgabe von Zeilen ohne \n am Ende
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

-- Datentyp fuer die Menge von Zeichen in einem Terminalfenster
-- der Groesse termRows x termCols

data ScreenDimension = ScreenDimension
  { termRows :: Int,
    termCols :: Int
  }
  deriving (Show)

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

-- Textprocessing

paginate :: ScreenDimension -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimension trows tcols) finfo txt =
  let wrappedLines = concatMap (softWrap tcols) $ Text.lines txt
      pageLines = groupsOf trows wrappedLines
      pages = map (Text.unlines . padTo trows) pageLines
      pageCount = length pages
      statusLines = map (formatFileInfo finfo tcols pageCount) [1 .. pageCount]
   in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad = take lineCount $ rowsToPad <> repeat ""

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

-- FileData

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWritable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  contents <- BS.readFile filePath
  let size = BS.length contents
  return
    FileInfo
      { filePath = filePath,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable perms,
        fileWritable = Directory.writable perms,
        fileExecutable = Directory.executable perms
      }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
  let timestamp =
        TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
      permissionString =
        [ if fileReadable then 'r' else '-',
          if fileWritable then 'w' else '-',
          if fileExecutable then 'x' else '-'
        ]
      statusLine =
        Text.pack $
          printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            filePath
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
   in invertText (truncateStatus statusLine)
  where
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth =
          Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine

-- Output

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page : pages) = do
  clearScreen
  TextIO.putStr page
  ch <- getContinue
  case ch of
    Continue -> showPages pages
    Cancel -> return ()

getContinue :: IO ContinueCancel
getContinue = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  ch <- getChar
  case ch of
    ' ' -> return Continue
    'q' -> return Cancel
    _ -> getContinue

invertText :: Text.Text -> Text.Text
invertText inputString =
  let reverseVideo = "\^[[7m"
      resetVideo = "\^[[0m"
   in reverseVideo <> inputString <> resetVideo

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

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

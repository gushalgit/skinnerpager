{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HCat (runHCat, paginate) where

-- R. Skinner, Kap. 8

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import qualified System.Info as SysInfo
import qualified System.IO.Error as IOError
import System.IO
import System.Process (readProcess)

runHCat :: IO ()
runHCat = do
  handleIOErr $
    handleArgs
      >>= eitherToError
      >>= readFile
      >>= putStrLn
  putStrLn "weiter? (Leerzeichen - ja, q - quit): "
  ans <- getContinue
  case ans of
    Continue -> do
      ts <- getTerminalSize
      print ts
    Cancel -> putStrLn "Ende"
    where
    handleIOErr :: IO () -> IO ()
    handleIOErr ioAction = Exception.catch ioAction $
      \e -> putStrLn "ran into an error:" >> print @IOError e

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        [] -> Left "Dateiname fehlt"
        _mehrereArgumente -> Left "es ist nur ein Dateiname erlaubt"

eitherToError :: (Show a) => Either a b -> IO b
eitherToError (Right x) = return x
eitherToError (Left e) =
  Exception.throwIO . IOError.userError $ show e

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
  -- ch <- hGetChar stdin
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
      return $ ScreenDimension lines' cols'
      
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
   in hd : groupsOf n tl

paginate :: ScreenDimension -> Text.Text -> [Text.Text]
paginate dim txt =
  let allLines = Text.lines txt
      wrappedLines = concatMap (softWrap (cols dim)) allLines
      pageLines = groupsOf (rows dim) wrappedLines
   in map Text.unlines pageLines

softWrap :: Int -> Text.Text -> [Text.Text]
softWrap maxlg line
  | Text.length line <= maxlg = [line]
  | otherwise =
      let (candidate, rest) = Text.splitAt maxlg line
          (firstPart, overflow) =
            if Text.head rest == ' '
              then (candidate, "")
              else splitOnFirstSpace candidate (maxlg - 1)
       in firstPart : softWrap maxlg (overflow <> rest)
  where
    splitOnFirstSpace cand textIdx
      | textIdx <= 0 = (cand, Text.empty)
      | Text.index cand textIdx == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIdx cand
           in (wrappedLine, Text.tail rest)
      | otherwise = splitOnFirstSpace cand (textIdx - 1)

bspt, bsp1 :: Text.Text
bspt = "Dieser Text ist nicht lang, aber er erfuellt seinen Zweck."
bsp1 = "word wrapping is tricky"

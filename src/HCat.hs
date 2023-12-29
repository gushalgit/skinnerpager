{-# LANGUAGE TypeApplications #-}

module HCat (runHCat) where

-- R. Skinner, Kap. 8

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import qualified System.IO.Error as IOError

runHCat :: IO ()
runHCat =
  handleIOErr $
    handleArgs
      >>= eitherToError
      >>= readFile
      >>= putStrLn
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
          (firstPart, overflow) = splitOnFirstSpace candidate (Text.length candidate - 1)
       in firstPart : softWrap maxlg (overflow <> rest)
  where
    splitOnFirstSpace cand textIdx
      | textIdx <= 0 = (cand, Text.empty)
      | Text.index cand textIdx == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIdx cand
           in (wrappedLine, Text.tail rest)
      | otherwise = splitOnFirstSpace cand (textIdx - 1)

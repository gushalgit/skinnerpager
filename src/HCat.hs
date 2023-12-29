{-# LANGUAGE TypeApplications #-}

module 
  HCat (runHCat,)
where
-- R. Skinner, Kap. 8

import qualified Control.Exception as Exception
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

eitherToError :: Show a => Either a b -> IO b
eitherToError (Right x) = return x
eitherToError (Left e) =
  Exception.throwIO . IOError.userError $ show e

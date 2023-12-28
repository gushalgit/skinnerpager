module HCat where

import qualified System.Environment as Env

runHCat :: IO ()
runHCat = handleArgs >>= print

handleArgs :: IO FilePath
handleArgs = head <$> Env.getArgs

module HCat where
-- R. Skinner, Kap. 8

import qualified System.Environment as Env

runHCat :: IO ()
runHCat = handleArgs >>= displayMessage 
  where
    displayMessage parsedArgs =
      case parsedArgs of
        Left errorMessage ->
          putStrLn $ "Error: " <> errorMessage
        Right filename ->
          putStrLn $ "Opening file: " <> filename

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        [] -> Left "Dateiname fehlt"
        _mehrereArgumente  -> Left "es ist nur ein Dateiname erlaubt"

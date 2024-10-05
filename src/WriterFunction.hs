module WriterFunction where

import System.IO ()
import Control.Exception (try, IOException)

--------------------------------------------------------------------------------------------------

writeToFile :: FilePath -> String -> IO ()
writeToFile path contents = do 
    result <- try (appendFile path contents) :: IO (Either IOException ())
    case result of 
        Left ex  -> error ((show $ ex) ++ ": unable to write to the file")
        Right () -> return ()

modifyStateVector :: String -> String
modifyStateVector state_k_list = replace_comma ++ "\n" 
    where
        without_brakets = filter (`notElem` "[]]") state_k_list
        replace_comma   = map (\c -> if c==',' then ' ' else c) without_brakets

--------------------------------------------------------------------------------------------------
module Main where

    import Data.Foldable
    import Data.Monoid
    import System.Environment
    
    import Options.Applicative as Opt
    
    import AppRWS
    import DiskUsage
    import FileCounter
    
    mkConfig :: Opt.Parser AppConfig
    mkConfig = AppConfig
               <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault) 
               <*> option auto (metavar "DEPTH" <> short 'd' <> long "depth"
                   <> value 0 <> showDefault <> help "Maximum depth of reporting ")
               <*> optional
                   (strOption (metavar "EXT" <> long "extension" <> short 'e'
                    <> help "Filter files by extension"))
    
    printLog :: Show s => AppLog s -> IO ()
    printLog = traverse_ printEntry 
      where
        printEntry (fp, s) = do
          putStr $ show s ++ "\t"
          putStrLn fp
    
    work :: AppConfig -> IO ()
    work config = do
      (_, xs) <- runBillingApp diskUsage config 0
      putStrLn "File space usage:"
      printLog xs
    
      (_, xs') <- runBillingApp countFile config 0
      putStrLn "File counter:"
      printLog xs'
      
    main :: IO ()  
    main = execParser opts >>= work
      where
        opts = info (mkConfig <**> helper)
                    (fullDesc <> progDesc "File space usage info")
    
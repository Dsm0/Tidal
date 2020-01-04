module Parser where

import Options.Applicative
import Data.Semigroup ((<>))

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

data Options = Options {
                        dontBootTidal :: Bool ,
                        dontBootSuperCollider :: Bool ,
                        bootP5jsDirt :: Bool ,
                        superColliderBootPath :: String ,
                        tidalBootPath :: String ,
                        sclangPath :: String,
                        ghciPath :: String,
                        nodePath :: String
                        }deriving(Show)

defaultOptions :: Options
defaultOptions = Options {
                        dontBootTidal = False,
                        dontBootSuperCollider = False,
                        bootP5jsDirt = False,
                        superColliderBootPath = "app/initScripts/startup.scd",
                        tidalBootPath = "app/initScripts/BootTidal.hs",
                        sclangPath = "sclang" ,
                        ghciPath = "ghci",
                        nodePath = "node"
                        }

flags :: Parser Options
flags = Options
      <$> switch
          ( long "dontBootTidal" -- by default, this value is false
         <> help "use this option if you don't want to boot tidal"
         )
      <*> switch
          ( long "dontBootSuperCollider" -- by default, this value is false
         <> help "use this option if you don't want to boot supercollider"
         )
      <*> switch
          ( long "bootP5jsDirt" -- by default, this value is false
         <> help "use this option if you want to boot p5jsDirt"
         )
      <*> strOption
          ( long "supercolliderBootPath"
         <> metavar "path/startup.scd"
         <> help "where to find startup.scd"
         <> showDefault
         <> value (superColliderBootPath defaultOptions) )
      <*> strOption
          ( long "tidalBootPath"
         <> metavar "path/BootTidal.hs"
         <> help "where to find BootTidal.hs"
         <> showDefault
         <> value (tidalBootPath defaultOptions) )
      <*> strOption
          ( long "sclangPath"
         <> metavar "path/sclang"
         <> help "where to find sclang"
         <> showDefault
         <> value (sclangPath defaultOptions) )
      <*> strOption
          ( long "ghciPath"
         <> help "where and how to execute ghci"
         <> showDefault
         <> value (ghciPath defaultOptions)
         <> metavar "path/ghci")
      <*> strOption
          ( long "nodePath"
         <> help "where to find node (only for use with p5jsDirt)"
         <> showDefault
         <> value "app/initScripts/node/p5jsdirt"
         <> metavar (nodePath defaultOptions))

optionParser :: IO (IO Options)
optionParser = return $ execParser opts
  where
    opts = info (flags <**> helper)
      ( fullDesc
     <> progDesc "control tidal and supercollider from one commandline"
     <> header "tidal (Dsm0's fork) - basically yaxu's tidalcycles with a slightly easier time booting" )

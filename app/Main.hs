{-# LANGUAGE MultiWayIf #-}

module Main where

import Parser as Parser
import qualified System.Process as Process
import Sound.Tidal.Context
import System.Environment
import System.Exit
import GHC.IO.Handle
import Data.Char (chr)
import Options.Applicative
import System.IO



exit_   = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

createTidalProcess :: String -> Process.CreateProcess
createTidalProcess ghciCommand = (Process.proc firstArg otherArgs) -- ["BootTidal.hs"]
                  { Process.std_in  = Process.CreatePipe
                  , Process.std_out = Process.Inherit
                  , Process.std_err = Process.Inherit
                  , Process.delegate_ctlc = True
                  }
                  where (firstArg:otherArgs) = words ghciCommand

createSupercolliderProcess:: String -> Process.CreateProcess
createSupercolliderProcess sclangCommand = (Process.proc firstArg ["-s"]) -- ["app/initScripts/startup.scd"]
                              { Process.std_in  = Process.CreatePipe
                              , Process.std_out = Process.Inherit
                              , Process.std_err = Process.Inherit
                              , Process.delegate_ctlc = False
                              }
                              where (firstArg:otherArgs) = words sclangCommand

main :: IO ()
main = do
  optionsIO <- Parser.optionParser
  options <- optionsIO
  let tidalBootPath' = Parser.tidalBootPath options
      superColliderBootPath' = Parser.superColliderBootPath options
      ghciPath' = Parser.ghciPath options
      sclangPath' = Parser.sclangPath options
      supercolliderProcess = case (dontBootSuperCollider options) of
        False -> createSupercolliderProcess $ sclangPath'
        True  -> (Process.proc "echo" []) { Process.std_in  = Process.CreatePipe, Process.std_out = Process.Inherit, Process.std_err = Process.Inherit}
      tidalProcess = case (dontBootTidal options) of
        False -> createTidalProcess $ ghciPath'
        True  -> (Process.proc "echo" []) { Process.std_in  = Process.CreatePipe, Process.std_out = Process.Inherit, Process.std_err = Process.Inherit}
  st@(stIn, stOut, stErr, stDel) <- doubleWithCreateProcess supercolliderProcess tidalProcess (stInteraction options)
  Process.cleanupProcess st

type ProcessReturns = (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
type ProcessHandler = Maybe Handle -> Maybe Handle -> Maybe Handle -> Process.ProcessHandle -> IO ProcessReturns
type DualProcessHandler = Process.CreateProcess -> Process.CreateProcess -> (ProcessReturns -> ProcessReturns -> IO ProcessReturns) -> IO ProcessReturns


doubleWithCreateProcess :: DualProcessHandler -- this function allows any 2 different Processes to interact with eachother via the passed function `interaction`
                                              -- it allows both the supercollider (sclang) and tidalcycles (ghci) processes to be controlled via one terminal
doubleWithCreateProcess process1 process2 interaction = do
  p1_ <- Process.withCreateProcess process1 (interaction_ process2)
  return p1_
    where interaction_ p a b c d = do
            p2_ <- Process.withCreateProcess p (interaction__ (a,b,c,d))
            return p2_
              where interaction__ abcd_ a' b' c' d' = interaction abcd_ (a',b',c',d')


stInteraction :: Parser.Options -> ProcessReturns -> ProcessReturns -> IO ProcessReturns
stInteraction options s'' t'' = do
  interaction_ s'' t''
    where interaction_ s''' t''' = do
            let (stdin',stdout',stderr',ph') = t'''
                (stdin'',stdout'',stderr'',ph'') = s'''
            let bootPath = Parser.tidalBootPath options
                scBootPath = Parser.superColliderBootPath options
                bootFile = ":script " ++ bootPath ++ " \n" -- command to laod BootTidal.hs in the ghci
                scBootFile = "\"" ++ scBootPath ++ "\".load \f" -- command to laod BootTidal.hs in the ghci
                mb = maybe (error "Maybe.fromJust: Nothing") id

                sendInput stdin_ cmd = do
                  hPutStrLn stdin_ $ "" ++ cmd
                  hFlush stdin_

                sendInputTidal = sendInput (mb stdin')
                sendInputSC = sendInput (mb stdin'')

            hSetBuffering (mb stdin') LineBuffering
            hSetBuffering (mb stdin'') LineBuffering

            hPutStr (mb stdin'') scBootFile -- passes BootTidal.hs to ghci as a script
            hFlush (mb stdin'') -- makes sure it evaluates (just in case)

            -- hReady (mb stdin')
            hPutStr (mb stdin') bootFile -- passes BootTidal.hs to ghci as a script
            hFlush (mb stdin') -- makes sure it evaluates (just in case)

            _ <- hWaitForInput stdin 1000
            let pipeUserInput = do -- this function takes user input and pipes it into the running tidalcycles process
                                   -- until the user calls ":quit"
                                   -- TODO: let the user input ctrl-d to stop the process
                      hReady stdin
                      command_ <- getLine
                      if  | (command_ == ":quit") -> do
                              sendInputTidal ":quit"
                              putStrLn "quitting"
                              return (stdout')
                          | (take 3 command_ == ":sc") -> do
                              -- TODO: evaluate arguments passed to :sc as haskell statements,
                              -- then have the output of those haskell statements be passed to sclang
                              -- this would allow for wrapping sclang functions as haskell functions

                              -- this likely involves creating a pipe for the ghci stdout
                              sendInputSC $ (drop 3 command_) ++ [chr 12]
                              pipeUserInput
                          | otherwise -> do
                              sendInputTidal command_
                              pipeUserInput
            __stdout__ <- pipeUserInput
            _ <- Process.waitForProcess ph'
            return (stdin', __stdout__,stderr',ph')

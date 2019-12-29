{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Process as Process
import qualified Sound.Tidal.Context
import System.Environment
import System.Exit
import GHC.IO.Handle
-- import System.IO as Sys
import System.IO (hPutStr,hPutStrLn, stdin, stdout, stderr,hSetEcho,hFlush,hReady)
import qualified Turtle



tidalProcess = (Process.proc "ghci" []) -- ["BootTidal.hs"]
                  { Process.std_in  = Process.CreatePipe
                  , Process.std_out = Process.Inherit
                  , Process.std_err = Process.Inherit
                  , Process.delegate_ctlc = True
                  }

supercolliderProcess args = (Process.proc "sclang" args ) -- ["app/initScripts/startup.scd"]
                              { Process.std_in  = Process.CreatePipe
                              , Process.std_out = Process.Inherit
                              , Process.std_err = Process.Inherit
                              , Process.create_new_console = True
                              , Process.delegate_ctlc = False
                              }

main = do
  let s = supercolliderProcess ["app/initScripts/startup.scd"]
      t = tidalProcess
  st@(stIn, stOut, stErr, stDel) <- doubleWithCreateProcess s t interaction
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
          let abcd = (a,b,c,d)
          p2_ <- Process.withCreateProcess process2 (interaction__ abcd)
          return p2_
            where interaction__ abcd_ a' b' c' d' = interaction abcd_ (a',b',c',d')


interaction :: ProcessReturns -> ProcessReturns -> IO ProcessReturns
interaction s' t' = do
  let (stdin',stdout',stderr',ph') = t'
      (scIn, scOut,scErr,scDel) = s'
  interaction_ stdin' stdout' stderr' ph'
    where interaction_ stdin' stdout' stderr' ph = do
            let bootPath = "app/initScripts/BootTidal.hs"
                bootFile = ":scipt" ++ bootPath ++ " \n" -- command to laod BootTidal.hs in the ghci
                mb = maybe (error "Maybe.fromJust: Nothing") id
            hPutStr (mb stdin') bootFile -- passes BootTidal.hs to ghci as a script
            hFlush (mb stdin') -- makes sure it evaluates (just in case)
            let pipeUserInput = do -- this function takes user input and pipes it into the running tidalcycles process
                                   -- until the user calls ":quit"
                                   -- TODO: let the user input ctrl-d to stop the process
                      command <- getLine
                      sendInput command
                      if (command == ":quit")
                        then do
                          return (stdout')
                        else do
                          pipeUserInput
                            where sendInput cmd = do { hPutStrLn (mb stdin') cmd ; hFlush (mb stdin')}
            stdout__ <- pipeUserInput
            return (stdin', stdout__,stderr',ph)

-- the functions below are for when I integrate command line arguments


main_ = do
  args <- getArgs
  parse args

main__ = getArgs >>= parse >>= putStr . tac

tac  = unlines . reverse . lines

parse ["-h"] = usage   >> exit_
parse ["-v"] = version >> exit_
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: p5jsDirt [-options]"
version = putStrLn "p5jsDirt version 0.0.1"
exit_    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

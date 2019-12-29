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

-- main = do
--   let s = supercolliderProcess ["app/initScripts/startup.scd"]
--   t@(tIn, tOut, tErr, tDel) <- Process.withCreateProcess tidalProcess (interaction s)
--   Process.cleanupProcess t

main = do
  let s = supercolliderProcess ["app/initScripts/startup.scd"]
      t = tidalProcess
  st@(stIn, stOut, stErr, stDel) <- doubleWithCreateProcess s t doubleInteraction
  Process.cleanupProcess st


type ProcessReturns = (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
type ProcessHandler = Maybe Handle -> Maybe Handle -> Maybe Handle -> Process.ProcessHandle -> IO ProcessReturns
type DualProcessHandler = Process.CreateProcess -> Process.CreateProcess -> (ProcessReturns -> ProcessReturns -> IO ProcessReturns) -> IO ProcessReturns



interactionWrap f (a,b,c,d) = f a b c d

doubleWithCreateProcess :: DualProcessHandler
doubleWithCreateProcess process1 process2 interaction = do
  p1_ <- Process.withCreateProcess process1 (interaction_ process2)
  return p1_
    where interaction_ p a b c d = do
          let abcd = (a,b,c,d)
          p2_ <- Process.withCreateProcess process2 (interaction__ abcd)
          return p2_
            where interaction__ abcd_ a' b' c' d' = interaction abcd_ (a',b',c',d')

            -- where interaction__ abcd_ a' b' c' d' = do
            --           let abcd' = (a',b',c',d')
            --           return interaction abcd_ abcd'

            -- interaction_ process in'p1 out'p1 err'p1 del'p1 = do
            --       let p1 = (in'p1, out'p1, err'p1, del'p1)
            --       return p2_
            --         where interaction__ :: ProcessHandler
            --               interaction__ p1' in'p2 out'p2 err'p2 del'p2 = do
            --                 let p2 = (in'p2, out'p2, err'p2, del'p2)
            --                 return $ (\x -> x >>= id) $ interaction p1' p2


-- doubleWithCreateProcess process1 process2 interaction = do
--   p1_ <- Process.withCreateProcess process1 (interaction_ process2)
--   return p1_
--     where interaction_ :: ProcessHandler
--           interaction_ process in'p1 out'p1 err'p1 del'p1 = do
--                 let p1 = (in'p1, out'p1, err'p1, del'p1)
--                 p2_ <- Process.withCreateProcess process2 (interaction__ p1)
--                 return p2_
--                   where interaction__ :: ProcessHandler
--                         interaction__ p1' in'p2 out'p2 err'p2 del'p2 = do
--                           let p2 = (in'p2, out'p2, err'p2, del'p2)
--                           return $ (\x -> x >>= id) $ interaction p1' p2


doubleInteraction :: ProcessReturns -> ProcessReturns -> IO ProcessReturns
doubleInteraction s' t' = do
  let (stdin',stdout',stderr',ph') = t'
      (scIn, scOut,scErr,scDel) = s'
  interaction_ stdin' stdout' stderr' ph'
    where interaction_ stdin' stdout' stderr' ph = do
            let str = "show 3 \n"
                mb = maybe (error "Maybe.fromJust: Nothing") id
            hPutStr (mb stdin') str
            hFlush (mb stdin')
            let pipeUserInput = do
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

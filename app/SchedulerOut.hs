module Main where

import           System.MQ.Scheduler (loadNetConfig, runSchedulerOut)

main :: IO ()
main = loadNetConfig >>= runSchedulerOut

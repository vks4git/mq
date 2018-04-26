module Main where

import           System.MQ.Scheduler (loadNetConfig, runSchedulerIn)

main :: IO ()
main = loadNetConfig >>= runSchedulerIn

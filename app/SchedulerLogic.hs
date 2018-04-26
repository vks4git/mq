module Main where

import           Control.Applicative (liftA2)
import           Control.Monad       (join)
import           System.MQ.Scheduler (loadLogicConfig, loadNetConfig,
                                      runSchedulerLogic)

main :: IO ()
main = join $ liftA2 runSchedulerLogic loadNetConfig loadLogicConfig

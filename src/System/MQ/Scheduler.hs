{-|

This module contains schedulers, that is central place in Monique.
Scheduler consists from 3 parts:
  * 'SchedulerIn'    - receives all messages from the world;
  * 'SchedulerLogic' - processes all messages with some logic;
  * 'SchedulerOut'   - publishes all message to the world.
Scheme:
@
             World | PUSH >
               |
  < PULL | SchedulerIn | PUSH >
               |
 < PULL | SchedulerLogic | PUSH > x N
               |
  < PULL | SchedulerOut | PUB >
               |
     < PUB | World
@
As you can see, 'SchedulerLogic' can be represented with many copies, while 'SchedulerIn' and 'SchedulerOut' have only one.

-}

module System.MQ.Scheduler
  ( module System.MQ.Scheduler.Internal.Config
  , module System.MQ.Scheduler.Internal.In
  , module System.MQ.Scheduler.Internal.Logic
  , module System.MQ.Scheduler.Internal.Out
  ) where

import System.MQ.Scheduler.Internal.Config
import System.MQ.Scheduler.Internal.In
import System.MQ.Scheduler.Internal.Logic
import System.MQ.Scheduler.Internal.Out 

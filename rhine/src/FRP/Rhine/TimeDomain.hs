{- |
This module defines the 'TimeDomain' class.
Its instances model time.
Several instances such as 'UTCTime', 'Double' and 'Integer' are supplied here.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.TimeDomain
  ( module FRP.Rhine.TimeDomain
  , UTCTime
  )
  where

-- time
import Data.Time.Clock (UTCTime, diffUTCTime)

{- |
A time domain is an affine space representing a notion of time,
such as real time, simulated time, steps, or a completely different notion.

Expected law:

@(t1 `diffTime` t3) `difference` (t1 `diffTime` t2) = t2 `diffTime` t3@
-}
class TimeDifference (Diff time) => TimeDomain time where
  type Diff time
  diffTime :: time -> time -> Diff time

-- | A type of durations, or differences betweens time stamps.
class TimeDifference d where
  difference :: d -> d -> d

instance TimeDomain UTCTime where
  type Diff UTCTime = Double
  diffTime t1 t2 = realToFrac $ diffUTCTime t1 t2

instance TimeDifference Double where
  difference = (-)

instance TimeDomain Double where
  type Diff Double = Double
  diffTime = (-)

instance TimeDifference Float where
  difference = (-)

instance TimeDomain Float where
  type Diff Float = Float
  diffTime = (-)

instance TimeDifference Integer where
  difference = (-)

instance TimeDomain Integer where
  type Diff Integer = Integer
  diffTime = (-)

instance TimeDifference () where
  difference _ _ = ()

instance TimeDomain () where
  type Diff () = ()
  diffTime _ _ = ()

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain { fromNumTimeDomain :: a }
  deriving Num

instance Num a => TimeDifference (NumTimeDomain a) where
  difference = (-)

instance Num a => TimeDomain (NumTimeDomain a) where
  type Diff (NumTimeDomain a) = NumTimeDomain a
  diffTime = (-)

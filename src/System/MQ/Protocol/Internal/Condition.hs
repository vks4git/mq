{-# LANGUAGE ExistentialQuantification #-}

module System.MQ.Protocol.Internal.Condition
  (
    Condition (..)
  , tautology
  , absurd
  , matches
  , itself
  ) where

-- | Conditional expressions over type 'a' and its mappings.
-- Supported operations:
-- * equality check  :==
-- * predicate check :?
-- * disunction      :&&
-- * conjunction     :||
--
-- Typical usage:
-- Say we have variable 'var :: a', a function 'f :: a -> b' and a value 'val :: b'.
-- Expression 'f :== b' acts as 'f a == b'
-- Examples:
--
-- > data D = D { fld1 :: Int
-- >            , fld2 :: String
-- >            , fld3 :: Double
-- >            }
-- >
-- > d = D 42 "noononno" 1.618
-- > d `matches` (fld1 :== 12 :&& fld2 :== "abc")
-- > False
-- >
-- > d `matches` (fld1 :== 42 :|| fld3 == 1.0)
-- > True
--
infix  4 :==
infix  4 :?
infixr 3 :&&
infixr 2 :||

data Condition a = forall b. Eq b => (a -> b) :== b
                 | forall b. (a -> b) :? (b -> Bool)
                 | Condition a :&& Condition a
                 | Condition a :|| Condition a
                 | Not (Condition a)

-- | Check whether data satisfies conditions on it.
--
matches :: a -> Condition a -> Bool
matches obj (transform :== ref)      = transform obj == ref
matches obj (transform :? predicate) = predicate . transform $ obj
matches obj (u :&& v)                = matches obj u && matches obj v
matches obj (u :|| v)                = matches obj u || matches obj v
matches obj (Not condition)          = not $ matches obj condition

-- | Matching 'tautology' will always succeed.
-- > whatever `matches` tautology == True
-- > -- Match is lazy:
-- > undefined `matches` tautology == True
--
tautology :: Condition a
tautology = const True :== True


-- | Matching 'absurd' will always fail.
-- > whatever `matches` absurd == False
--
absurd :: Condition a
absurd = Not tautology


-- | Object itself instead of its mappings is matched with help of this alias.
-- > 42 `matches` (itself :== 42) == True
-- > 42 `matches` (itself :== 41) == False
--
itself :: a -> a
itself = id

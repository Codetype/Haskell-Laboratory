{-
-
-Dequeue ADT
-
-}

module Dequeue
  ( Dequeue
  , emptyDEQ       -- :: Dequeue a
  , isEmptyDEQ     -- :: Dequeue a -> Bool
  , lengthDEQ      -- :: Dequeue a -> Int
  , firstDEQ       -- :: Dequeue a -> Maybe a
  , lastDEQ        -- :: Dequeue a -> Maybe a
  , takeFrontDEQ   -- :: Int -> Dequeue a -> [a]
  , takeBackDEQ    -- :: Int -> Dequeue a -> [a]
  , pushFrontDEQ   -- :: Dequeue a -> a -> Dequeue a
  , popFrontDEQ    -- :: Dequeue a -> Maybe(a, Dequeue a)
  , pushBackDEQ    -- :: Dequeue a -> a -> Dequeue a
  --, popBackDEQ
  ) where

emptyDEQ     :: Dequeue a
isEmptyDEQ   :: Dequeue a -> Bool
lengthDEQ    :: Dequeue a -> Int
firstDEQ     :: Dequeue a -> Maybe a
lastDEQ      :: Dequeue a -> Maybe a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ  :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ  :: Dequeue a -> Maybe(a ,Dequeue a)
pushBackDEQ  :: Dequeue a -> a -> Dequeue a
--popBackDEQ :: Dequeue a -> Maybe(a, Dequeue a)

--implementation
newtype Dequeue a = MkDequeue [a] deriving Show

emptyDEQ = MkDequeue []
isEmptyDEQ (MkDequeue dq) = null dq
lengthDEQ (MkDequeue dq) = length dq
firstDEQ (MkDequeue dq) = if isEmptyDEQ (MkDequeue dq)
   then Nothing
   else Just (head dq)
lastDEQ (MkDequeue dq) = if isEmptyDEQ (MkDequeue dq)
    then Nothing    
    else Just (last dq)
takeFrontDEQ x (MkDequeue dq) =  take x dq
takeBackDEQ x (MkDequeue dq) = drop (length dq - x) dq
pushFrontDEQ (MkDequeue dq) x = MkDequeue (x:dq)

popFrontDEQ (MkDequeue dq) = if isEmptyDEQ (MkDequeue dq)
                           then Nothing
                           else Just (head dq, MkDequeue(tail dq))

popBackDEQ (MkDequeue dq) = if isEmptyDEQ (MkDequeue dq)
                           then Nothing
                           else Just (last dq, MkDequeue (takeFrontDEQ ((length dq) - 1) (MkDequeue dq) ))   

pushBackDEQ (MkDequeue dq) x = MkDequeue (dq++[x])

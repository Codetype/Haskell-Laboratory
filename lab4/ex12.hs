{-
-
-Dequeue ADT
-
-}

module Dequeue
  (  Dequeue
  ,  emptyDEQ       -- :: Dequeue a
  ,  isEmptyDEQ     -- :: Dequeue a -> Bool 
  ,  lengthDEQ      -- :: Dequeue a -> Int
  ,  firstDEQ       -- :: Dequeue a -> Maybe a
  ,  lastDEQ        -- :: Dequeue a -> Maybe a
  ,  takeFrontDEQ   -- :: Int -> Dequeue a -> [a]
  ,  takeBackDEQ    -- :: Int -> Dequeue a -> [a]
  ,  pushFrontDEQ   -- :: Dequeue a -> a -> Dequeue a
  ,  popFrontDEQ    -- :: Dequeue a -> Maybe(a, Dequeue a)
  ,  pushBackDEQ    -- :: Dequeue a -> a -> Dequeue a
  ) where
 
emptyDEQ     :: Dequeue a
isEmptyDEQ   :: Dequeue a -> Bool
lengthDEQ    :: Dequeue a -> Int
firstDEQ     :: Dequeue a -> Maybe a
lastDEQ      :: Dequeue a -> Maybe a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ  :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ  :: Dequeue a -> Maybe(a, Dequeue a)
pushBackDEQ  :: Dequeue a -> a -> Dequeue a

--implementation
data Dequeue a = Dequeue {
  inbox  :: [a],
  outbox :: [a]
} deriving (Eq, Show)

emptyDEQ = Dequeue {inbox = [], outbox = []}
isEmptyDEQ (Dequeue inb out) = null inb && null out
lengthDEQ (Dequeue inb out) = length inb + length out 
firstDEQ (Dequeue inb _) = if null inb
     then Nothing
     else Just (head inb)
lastDEQ (Dequeue _ out) = if null out
     then Nothing
     else Just (head out)
takeFrontDEQ x (Dequeue inb _) = take x inb
takeBackDEQ x (Dequeue _ out) = take x out
pushFrontDEQ (Dequeue inb out) e = Dequeue (e:inb) out
popFrontDEQ (Dequeue inb out) = if null inb
     then Nothing
     else Just (head inb, (Dequeue (tail inb) out))
pushBackDEQ  (Dequeue inb out) e = Dequeue inb (e:out)
popBackDEQ (Dequeue inb out) = if null out
     then Nothing
     else Just (head out, (Dequeue inb (tail out)))

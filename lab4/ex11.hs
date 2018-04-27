data Queue a = Queue {
  inbox :: [a],
  outbox :: [a]
} deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push e (Queue inb out) = Queue(e:inb) out

pop :: Queue a -> (a, Queue a)
pop q@(Queue inb []) = pop $ Queue [] (reverse inb)
pop (Queue inb outb) = (head outb, Queue inb (tail outb))

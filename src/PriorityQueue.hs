module PriorityQueue (PriorityQueue, insert, least, exists, remove) where

type PriorityQueue a = [(Float, a)]

insert :: PriorityQueue a -> (Float, a) -> PriorityQueue a
insert [] (n, v) = [(n, v)]
insert ((n1, v1):pq) (n2, v2)
  | n1<n2 = (n1, v1):insert pq (n2,v2)
  | otherwise = (n2, v2):(n1, v1):pq

least :: PriorityQueue a -> (a, PriorityQueue a)
least pq = (snd(head pq), tail pq)

exists :: (Eq a) => PriorityQueue a -> (Float, a) -> Bool
exists pq (n, a)
  | (n,a) == head pq = True
  | otherwise = exists (tail pq) (n, a)

remove :: (Eq a) => PriorityQueue a -> (Float, a) -> PriorityQueue a
remove [] _ = []
remove pq (n, a)
  | (n,a) == head pq = tail pq
  | otherwise        = remove (tail pq) (n, a)

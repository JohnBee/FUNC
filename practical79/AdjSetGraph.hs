module AdjSetGraph (Graph, module GraphSpec) where

import GraphSpec
import ListSet as S

-- invariant for G adjs:
-- A v ws `member` adjs && w `member` ws
-- ==>
-- A w vs `member` adjs && v `member` vs

data Graph a  =  G (Set (Adj a))

data Adj a    =  A a (Set a)

instance Eq a => Eq (Adj a) where
  A v _ == A w _  =  v == w

instance Ord a => Ord (Adj a) where
  compare (A v _) (A w _)  =  compare v w

vertex :: a -> Adj a
vertex x  =  A x undefined

instance GraphSpec Graph where
  empty                 =  G S.empty

  addVert v (G adjs)    =  G (add (A v S.empty) adjs)

  addEdge v w (G adjs)  =  G (addTo adjs)
    where
    addTo  =  addOrUpdate (A v (singleton w))
                          (\(A v vs) -> A v (add w vs)) .
              addOrUpdate (A w (singleton v))
                          (\(A w ws) -> A w (add v ws))

  delVert v (G adjs)  =  G (delIn (delOut adjs))
    where
    delIn    =  foldr1 (.)
                  [ addOrUpdate (vertex w) (\(A w xs) -> A w (delete v xs))
                  | w <- elements ws ]
    delOut   =  delete (vertex v)
    A _ ws   =  match  (vertex v) adjs

  delEdge v w (G adjs)  =  G (delFrom adjs)
    where
    delFrom  =  addOrUpdate (vertex v) (\(A v vs) -> A v (delete w vs)) .
                addOrUpdate (vertex w) (\(A w ws) -> A w (delete v ws))
  
  vertices (G adjs)  =  [v | A v _ <- elements adjs]

  adj v (G adjs)  =  elements vs
    where A _ vs  =  match (vertex v) adjs


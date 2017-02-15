import Data.Graph

bounds :: Bounds
bounds = (1,8)

edge :: [Edge]
edge = [(1,2),(1,8),(2,1),(3,2),(4,3),(5,4),(5,6),(6,5),(6,7),(7,6),(7,8),(8,7),(8,1)]

playerTile :: Vertex
playerTile = 5

allVertices :: Graph -> [Vertex]
allVertices g = vertices g

allEdges :: Graph -> [Edge]
allEdges g = edges g

computerTile :: Vertex
computerTile = 1

reachableByComputer :: Graph -> Vertex -> [Vertex]
reachableByComputer g cTile = reachable g cTile

canPursuit :: Graph -> Vertex -> Vertex -> Bool
canPursuit g cTile pTile = path g cTile pTile

build :: Graph
build = buildG bounds edge

addToClosedSet :: [Vertex] -> Vertex -> [Vertex]
addToClosedSet set tile = tile:set

removeFromOpenSet :: [Vertex] -> Vertex -> [Vertex]
removeFromOpenSet set tile
  | tile == head set = tail set
  | otherwise        = head set: removeFromOpenSet (tail set) tile
  

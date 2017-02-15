import Data.Graph
data Tile = Tile Vertex (Int, Int) deriving (Show)
data Map = Map Graph Tile Tile

build :: (Int,Int) -> [Edge] -> Graph
build bounds edges = buildG bounds edges

allVertices :: Graph -> [Vertex]
allVertices g = vertices g

allEdges :: Graph -> [Edge]
allEdges g = edges g

reachableByComputer :: Graph -> Vertex -> [Vertex]
reachableByComputer g cTile = reachable g cTile

canPursuit :: Graph -> Vertex -> Vertex -> Bool
canPursuit g cTile pTile = path g cTile pTile

extractVertex :: Tile -> Vertex
extractVertex (Tile v (a,b)) = v

connectionsInTile :: Tile -> Graph -> [Edge]
connectionsInTile (Tile v _)  g = let edges = allEdges g
                     in
                       filter (\x-> (snd x)==v) edges

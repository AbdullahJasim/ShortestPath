{-----------------------------------------------------------------------------------------------------------
	Shortest Distance Implementation in Haskell
	Abdullah Jasim
	abdullah.m.jasim@gmail.com

	Input: 
		1. A beginning point and an ending point
		2. A list of links, each has the name of the 2 nodes it links and its cost / length

	Output:
		A list of points that you can traverse starting from the beginning point
			to reach the ending point in the cheapest / shortest distance possible

	Logic:
		1. Use depth first to get every possible path from beginning node to end node
		2. Store every path that ends with the ending node
			The path info contains the nodes it has traversed so far as well as the total cost
		3. Get the shortest one
	
	Data types:
		1. Given elements for the graph: links (and points can be taken from them)
		2. Calculated elements such as the distance between any 2 nodes and a path of nodes and its length
	
	Functions:
		1. iterateLinks: a function to iterate through the links of a single point
		2. getLink: function to take a list of links, a node, find the first link for the node and return the rest
			of the links
		3. 
-----------------------------------------------------------------------------------------------------------}

--Data type for point, synonym to a string
--Data type for a link, two points which are linked together and the length
--Integets will be considered in this code (for simplicity), but can work for floats
data Graph = Point String | Link (String, String, Integer) deriving Show

--Data type for the stored paths that go from the beginning point to the end point
data Table = Path ([String], Integer) deriving Show

--Input:
--Current point a
--End point b
--List of all initially given links to pass through for next node
--List of remaining unchecked links for current node
--The path traversed so far
--The list of paths to return at the end
iterateLinks :: (Graph, Graph, [Graph], [Graph], Table, [Table]) -> [Table]

--Base case: no more links to check for this node, simply return the table without modification
iterateLinks (Point a, Point b, initialLinks, [], path, table) = table

--In case we're at the target node, then store the path into the table
--If not, then traverse deeper into the graph
iterateLinks (Point a, Point b, initialLinks, nodeLinks, Path (listOfNodes, value), table) = case a == b of
 True -> table ++ [Path (listOfNodes, value)]
 False -> let
  (Point nextPoint, length, remainingLinks) = getLink (Point a, nodeLinks)
  newPath = (listOfNodes ++ [nextPoint], value + length)
   in iterateLinks (Point nextPoint, Point b, initialLinks, initialLinks, Path newPath, table)

getLink :: (Graph, [Graph]) -> (Graph, Integer, [Graph])

--Base case, no more links to iterate through, return an empty point with 0 length
getLink (Point a, []) = (Point "", 0, [])

getLink (Point a, Link (nodeA, nodeB, length) : links) = case a == nodeA of
 True -> (Point nodeB, length, links)
 False -> getLink (Point a, links)

getMin :: ([Table], Table) -> Table
getMin ([], path) = path

getMin(Path (listOfNodes, value) : paths, Path(storedPoints, storedValue)) = case value < storedValue of
 True -> getMin(paths, Path(listOfNodes, value))
 False -> getMin(paths, Path(storedPoints, storedValue))

--Function to find the shortets path between 2 nodes
--Returns a list of node to traverse and the cost of the path
--Input:
--Starting point
--End point
--List of links in the graph, each link being 2 points and a length
getShortestPath :: (Graph, Graph, [Graph]) -> Table

getShortestPath (Point a, Point b, links) = let
 table = iterateLinks(Point a, Point b, links, links, Path ([a], 0), [])
 shortestPath = getMin(table, Path ([], 9999999))
  in shortestPath
----------------------------------------------------------------------TESTS---------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------
--iterateLinks (Point "a", Point "c", [Link ("a", "b", 1), Link ("b", "c", 2)], [Link ("a", "b", 1), Link ("b", "c", 2)], Path ([], 0), [])
--iterateLinks (Point "a", Point "e", [Link ("a", "b", 1), Link ("b", "c", 2), Link ("b", "d", 3), Link ("c", "e", 5), Link ("d", "e", 3)], [Link ("a", "b", 1), Link ("b", "c", 2), Link ("b", "d", 3), Link ("c", "e", 5), Link ("d", "e", 3)], Path ([], 0), [])
--iterateLinks (Point "b", Point "e", [Link ("a", "b", 1), Link ("b", "c", 2), Link ("b", "d", 3), Link ("c", "e", 5), Link ("d", "e", 3)], [Link ("a", "b", 1), Link ("b", "c", 2), Link ("b", "d", 3), Link ("c", "e", 5), Link ("d", "e", 3)], Path (["b"], 1), [])
--getLink(Point "a", [Link ("a", "b", 1), Link ("b", "c", 2), Link ("b", "d", 3), Link ("c", "e", 5), Link ("d", "e", 3)])
--getLink (Point "b", [Link ("a", "b", 1), Link ("b", "c", 2), Link ("b", "d", 3), Link ("c", "e", 5), Link ("d", "e", 3)])
--getShortestPath(Point "a", Point "e", [Link ("a", "b", 1), Link ("b", "c", 2), Link ("b", "d", 3), Link ("c", "e", 5), Link ("d", "e", 3)])
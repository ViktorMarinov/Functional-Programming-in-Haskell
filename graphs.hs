type Node = String
type Graph = [(Node, [Node])]
type Path = [Node]

graph1:: Graph
graph1 = [("a",["b","d"]),("b",["c", "d"]),("c",["b"]), ("d",["c","e"]),("e",["a","f"]),("f",[])]

assoc :: Eq a => a -> [(a,[b])] -> (a,[b])
assoc key [] = (key,[])
assoc key (x:xs)
	| fst x==key = x
	| otherwise = assoc key xs

successors :: Node -> Graph -> [Node]
successors node graph = snd (assoc node graph)

gen_next :: Path -> Graph -> [Path]
-- Връща като резултат списък от продълженията
-- на даден път.
gen_next path graph = map (\x -> (path ++ [x])) (successors (last path) graph)

generate_paths :: [Path] -> Graph -> [Path]
-- Връща като резултат списък от продълженията
-- на пътищата от даден списък.
generate_paths paths graph = concat (map (\x -> gen_next x graph) paths)
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe
import Debug.Trace
import RollTheBall
import Pipes

import Debug.Trace
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node
				{ state :: s
				, action :: Maybe a
				, parent :: Maybe (Node s a)
				, depth :: Int
				, children :: [Node s a]
				} deriving (Eq, Ord, Show)
			

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState node = state node

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node= parent node

nodeDepth :: Node s a -> Int
nodeDepth node = depth node

nodeAction :: Node s a -> Maybe a
nodeAction node = action node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = children node

{- b
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a, Eq s, Show s, Show a) => s -> Node s a
createStateSpace stateStart = node
	where	
		node = Node stateStart Nothing Nothing 0 childrenList
		childrenList = map (createChildren 1 [stateStart] node) (successors stateStart)


createChildren :: (ProblemState s a, Eq s, Show s, Show a) => Int -> [s] -> Node s a -> (a, s)-> Node s a
createChildren i visited parentNode (act, level) = if notEndNode then intNode else trace ("No valid neighbours") endNode 
	where		
		toBeFiltered = successors level
		filteredChildren = filter (\p -> (snd p) `notElem` visited) toBeFiltered
		newVisited = [level]
		notEndNode = length filteredChildren /= 0
		newI = i + 1
		intNode = (Node level (Just act) (Just parentNode) i childrenList)
		childrenList = (map (createChildren newI newVisited intNode) filteredChildren)
		endNode = (Node level (Just act) (Just parentNode) i [])
		

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = (border, border) : (bfsAux (head border) (tail border))
	where
		border = nodeChildren node

bfsAux :: (Eq s, Ord s) => Node s a -> [Node s a] -> [([Node s a], [Node s a])]
bfsAux node lastBorder = (childrenNode, newBorder) : (bfsAux (head newBorder) (tail newBorder))
	where
		childrenNode = nodeChildren node
		newBorder = lastBorder ++ childrenNode
		


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: (Eq a, Ord s, Show s) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = (first, second)
	where
		firstBfs = bfs start
		secondBfs = bfs end
		bordersStart = map (\p -> (fst p)) firstBfs
		bordersEnd = map (\p -> (snd p)) secondBfs
		bordersStartState = map (\sublist -> map (\p -> nodeState p) sublist) bordersStart
		bordersEndState = map (\sublist -> map (\p -> nodeState p) sublist) bordersEnd
		commonList = zip bordersStartState bordersEndState
		intersectPerLevel = map (\(xs, ys) -> [ x | x <- xs, elem x ys]) commonList
		commonIndex = length $ takeWhile (\p -> length p == 0) intersectPerLevel
		commonNodeState = head $ intersectPerLevel !! commonIndex
		goodStartBorder = bordersStart !! commonIndex
		goodEndBorder = bordersEnd !! commonIndex
		first = head $ filter (\p -> nodeState p == commonNodeState) goodStartBorder
		second = head $ filter (\p -> nodeState p == commonNodeState) goodEndBorder

		

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath node = reverse retList
	where
		nodeList = span (\m -> isJust (nodeParent m)) (iterate (\n -> fromJust (nodeParent n)) node)
		goodNodeList = fst nodeList ++ [head (snd nodeList)]
		retList = map (\n -> (nodeAction n, nodeState n)) goodNodeList
		



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

extractPath2 :: (ProblemState s a, Eq a, Eq s, Show a, Show s) => Node s a -> Node s a -> [(Maybe a, s)] 
extractPath2 node child = retList
	where
		nodeList = span (\(n, _) -> isJust (nodeParent n)) (iterate (\(n, _) -> (fromJust (nodeParent n), n)) (node, child))
		goodNodeList = fst nodeList ++ [head (snd nodeList)]
		retList = map (\(n, m) -> ( Just (fst (reverseAction ((fromJust (nodeAction m)), nodeState n))), nodeState n)) goodNodeList




solve :: (ProblemState s a, Eq s, Ord s, Show s, Show a, Eq a)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve startState endState = (trace (show (map (\x -> fst x) fullPath))) fullPath
	where
		start = createStateSpace startState
		end = createStateSpace endState
		nodeStart = fst (bidirBFS start end)
		nodeEnd = snd (bidirBFS start end)
		firstPath = extractPath nodeStart
		secondPath = extractPath2 (fromJust (nodeParent nodeEnd)) nodeEnd
		fullPath = firstPath ++ secondPath
		

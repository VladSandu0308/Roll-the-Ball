{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import qualified Data.Array as A
import Debug.Trace

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

			
data Cell = Cell {c::Char,  p::Position}	
	deriving (Eq, Ord)--TODO
	
instance Show Cell where
	show (Cell cl _) = [cl]
		
data A = MakeA String deriving (Show, Eq, Ord)
data B = MakeB String deriving Show
data C = MakeC String deriving (Show, Eq)

f :: Int -> Int -> Int
f x y = x

class Addable t where
 add :: t a -> t a -> t a

instance Addable [] where
	add l1 l2 = zipWith (+) l1 l2
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Empty | Level (A.Array Position Cell) --TODO
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level where
	show (Level cells) = [endl] ++ foldl printCell "" list
		where
			list = A.elems cells
			printCell acc cell = acc ++ show cell
					

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (x, y) = Level goodBoard
	where
		board = A.array ((0, 0), (x, y + 1)) [((i, j), (Cell emptySpace (i,j))) | i <- [0..x], j <- [0..y+1]]
		goodBoard = board A.// [((i, y + 1), (Cell endl (i,y + 1))) | i <- [0..x]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (cl, (x,y)) (Level board) 
	| x >= 0 && y >= 0 && x <= fst (snd (A.bounds board)) && y <= snd (snd (A.bounds board)) 		= Level $ board A.// [((x,y), (Cell cl (x,y)))]
	| otherwise				= Level board



{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel size cells = foldr addCell (emptyLevel size) cells


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

isValidMove :: Directions -> Level -> Position -> Bool
isValidMove dir (Level board) (i,j)
	| fst destCellIndex < 0 || fst destCellIndex > 	fst (snd (A.bounds board))		= False
	| snd destCellIndex < 0 || snd destCellIndex >=	snd (snd (A.bounds board)) 		= False
	| destCell /= (Cell emptySpace destCellIndex)  									= False	
	| tStartCell == endl || tStartCell == emptySpace								= False
	| fixedCell						 												= False
	| otherwise						 												= True
		where
			destCellIndex = destIndex (i,j) dir
			destCell = board A.! destCellIndex
			startCell = board A.! (i,j)
			tStartCell = c $ startCell
			fixedCellStart = startCell == (Cell startUp (i,j)) || startCell == (Cell startDown (i,j)) ||
										startCell == (Cell startLeft (i,j)) || startCell == (Cell startRight (i,j))
			fixedCellWin = startCell == (Cell winUp (i,j)) || startCell == (Cell winDown (i,j)) ||
										startCell == (Cell winLeft (i,j)) || startCell == (Cell winRight (i,j))
			fixedCell = fixedCellStart || fixedCellWin


destIndex :: Position -> Directions -> Position
destIndex (i,j) dir = case dir of
	North		-> (i - 1,j)
	South		-> (i + 1,j)
	West		-> (i, j - 1)
	East		-> (i, j + 1)

moveCell :: Position -> Directions -> Level -> Level
moveCell point@(i,j) dir (Level board) = if (isValidMove dir lvl point) then (Level final) else lvl
		where
			lvl = (Level board)
			destCellIndex = destIndex (i,j) dir
			destCell = board A.! destCellIndex
			startCell = board A.! (i,j)
			tDestCell = c $ destCell
			tStartCell = c $ startCell
			auxBoard = board A.// [(destCellIndex, (Cell tStartCell destCellIndex))]
			final = auxBoard A.// [((i,j), (Cell tDestCell (i,j)))]

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}
upCheck::Cell -> Cell -> Bool
upCheck (Cell _ (x1,_)) (Cell c2 (x2, _))
	| x2 + 1 /= x1  = False
	| otherwise		= c2 == verPipe || c2 == topLeft || c2 == topRight || c2 == winDown 
	
downCheck::Cell -> Cell -> Bool
downCheck (Cell _ (x1,_)) (Cell c2 (x2, _))
	| x2 - 1 /= x1  = False
	| otherwise		= c2 == verPipe || c2 == botLeft || c2 == botRight || c2 == winUp
	
leftCheck::Cell -> Cell -> Bool
leftCheck(Cell _ (_,y1)) (Cell c2 (_, y2)) 
	| y2 + 1 /= y1  = False
	| otherwise		= c2 == horPipe || c2 == botLeft || c2 == topLeft || c2 == winRight	
					
rightCheck::Cell -> Cell -> Bool
rightCheck (Cell _ (_,y1)) (Cell c2 (_, y2))  
	| y2 - 1 /= y1  = False
	| otherwise		= c2 == horPipe || c2 == botRight || c2 == topRight || c2 == winLeft

connection :: Cell -> Cell -> Bool
connection cell1@(Cell c1 _) cell2
	| (c1 == startUp)		=	 upCheck cell1 cell2
	| (c1 == startDown)		=	downCheck cell1 cell2
	| (c1 == startLeft)		=	downCheck cell1 cell2
	| (c1 == startRight)	=	rightCheck cell1 cell2
	| (c1 == winDown || c1 == winLeft || c1 == winRight || c1 == winUp)	= trace ("WIN REACHED")True
	| (c1 == topLeft)		=	downCheck cell1 cell2 || rightCheck cell1 cell2
	| (c1 == topRight)		=	downCheck cell1 cell2 || leftCheck cell1 cell2
	| (c1 == botLeft)		=	upCheck cell1 cell2 || rightCheck cell1 cell2
	| (c1 == botRight)		=	upCheck cell1 cell2 || leftCheck cell1 cell2
	| (c1 == verPipe)		=	upCheck cell1 cell2 || downCheck cell1 cell2
	| (c1 == horPipe)		=	leftCheck cell1 cell2 || rightCheck cell1 cell2
	| otherwise				= 	False
					
				
{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
checkNear :: Cell -> Level -> (A.Array Position Int) -> Bool
checkNear cell@(Cell cl (x,y)) lvl@(Level board) visited = let
	winCell = cl == winDown || cl == winLeft || cl == winRight || cl == winUp
	goLeft
		| y == 0 	= False
		| connection cell (board A.! (x, y - 1)) && visited A.! (x, y - 1) == 0	=  checkNear (board A.! (x, y - 1)) lvl (visited A.// [((x,y), 1)]) 
		| otherwise	= False
	goRight
		| y == snd (snd (A.bounds board)) - 1 	= False
		| connection cell (board A.! (x, y + 1)) && visited A.! (x, y + 1) == 0	= checkNear (board A.! (x, y + 1)) lvl (visited A.// [((x,y), 1)])
		| otherwise	= False
	goUp
		| x == 0 	= False
		| connection cell (board A.! (x - 1, y)) && visited A.! (x - 1, y) == 0		=  checkNear (board A.! (x - 1, y)) lvl (visited A.// [((x,y), 1)])
		| otherwise	= False
	goDown
		| x == fst (snd (A.bounds board)) 	= False
		| connection cell (board A.! (x + 1, y)) && visited A.! (x + 1, y) == 0	 =  checkNear (board A.! (x + 1, y)) lvl (visited A.// [((x,y), 1)])
		| otherwise	= False
	in   if winCell then  True else  (goLeft || goRight || goUp || goDown)
		


wonLevel :: Level -> Bool
wonLevel lvl@(Level board ) = (checkNear startCell lvl visited)
	where 
		visited = A.array ((0, 0), (x, y)) [((i, j), 0) | i <- [0..x], j <- [0..y]]
		y = snd (snd (A.bounds board))
		x = fst (snd (A.bounds board))
		startCell = head (filter condition (A.elems board))
		condition cell = (c cell) == startUp || (c cell) == startDown || 
						 (c cell) == startLeft || (c cell) == startRight

							

instance ProblemState Level (Position, Directions) where
    successors lvl@(Level board) = nextSucc
		where
			lvlIndexes = A.indices board
			northMove = filter (isValidMove North lvl) lvlIndexes
			northAdd = map (\coords -> ((coords, North), (moveCell coords North lvl))) northMove
			southMove = filter (isValidMove South lvl) lvlIndexes
			southAdd = map (\coords -> ((coords, South), (moveCell coords South lvl))) southMove
			westMove = filter (isValidMove West lvl) lvlIndexes
			westAdd = map (\coords -> ((coords, West), (moveCell coords West lvl))) westMove
			eastMove = filter (isValidMove East lvl) lvlIndexes
			eastAdd = map (\coords -> ((coords, East), (moveCell coords East lvl))) eastMove
			nextSucc = northAdd ++ southAdd ++ westAdd ++ eastAdd

			
			
	
    isGoal lvl = wonLevel lvl
	
    reverseAction (((x,y), dir), lvl) = case dir of
		North		-> ((southCoords, South), (moveCell southCoords South lvl))
		South		-> ((northCoords, North), (moveCell northCoords North lvl))
		East		-> ((westCoords, West), (moveCell westCoords West lvl))
		West		-> ((eastCoords, East), (moveCell eastCoords East lvl))
		where
			southCoords = (x - 1, y)
			northCoords = (x + 1, y)
			westCoords = (x, y + 1)
			eastCoords = (x, y - 1)
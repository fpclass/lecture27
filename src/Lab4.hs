--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 4: User-defined types                                                  --
--------------------------------------------------------------------------------

module Lab4 where

--------------------------------------------------------------------------------
-- Red-black trees

data Colour = Red | Black

instance Show Colour where
    show Red   = "Red"
    show Black = "Black"

data Tree a = Leaf | Node Colour (Tree a) a (Tree a)
    deriving Show

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton x = Node Red empty x empty

makeBlack :: Tree a -> Tree a
makeBlack (Node _ l x r) = Node Black l x r
makeBlack tree           = tree

depth :: Tree a -> Int
depth Leaf           = 0
depth (Node _ l _ r) = 1 + max (depth l) (depth r)

toList :: Tree a -> [a]
toList Leaf           = []
toList (Node _ l x r) = toList l ++ [x] ++ toList r

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node _ l y r)
    | x == y    = True
    | x < y     = member x l
    | otherwise = member x r

balance :: Colour -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance c l x r = Node c l x r

insert :: Ord a => Tree a -> a -> Tree a
insert tree x = makeBlack (ins tree)
    where ins Leaf = singleton x
          ins (Node c l y r)
            | x<y       = balance c (ins l) y r
            | y<x       = balance c l y (ins r)
            | otherwise = Node c l y r

--------------------------------------------------------------------------------

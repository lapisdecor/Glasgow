data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftsubtree rightsubtree) = 1 + max (treeDepth leftsubtree) (treeDepth rightsubtree)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x leftsubtree rightsubtree) = x + treeSum leftsubtree + treeSum rightsubtree

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _  = True
isSortedTree (Node x leftsubtree rightsubtree) minVal maxVal =
    let leftSorted = isSortedTree leftsubtree minVal x
        rightSorted = isSortedTree rightsubtree x maxVal
    in x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
-- add a new max element to tree
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

insertValue :: Int -> Tree -> Tree
insertValue x Leaf = Node x Leaf Leaf      -- input tree with no nodes
insertValue x (Node y Leaf Leaf) =         -- input tree with one node
    if x <= y
    then
        Node y (Node x Leaf Leaf) Leaf 
    else
        Node y Leaf (Node x Leaf Leaf)
	
insertValue x (Node y leftSide rightSide) = -- input tree with multiple nodes
    if x <= y
    then
        Node y (insertValue x leftSide) rightSide
    else
        Node y leftSide (insertValue x rightSide)


convertTree :: Tree -> [Int]
convertTree Leaf = []
convertTree (Node x Leaf Leaf) = [x]
convertTree (Node x leftSide rightSide) = x:((convertTree leftSide) ++ (convertTree rightSide))
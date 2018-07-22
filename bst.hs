module BST  where
data BinarySearchTree a = NIL | Node (BinarySearchTree a) Int (BinarySearchTree a) deriving (Eq,Show)

empty :: (BinarySearchTree a) -> Bool
empty NIL = True
empty _ = False

treeSize :: (BinarySearchTree a) -> Int
treeSize NIL = 0
treeSize (Node left current right) = 1 + treeSize left + treeSize right

--operações básicas 

search :: (BinarySearchTree a) -> Int -> Bool
search NIL _ = False
search (Node left current right) node
 | current == node = True
 | node < current = search left node
 | node > current = search right node 

insert :: (BinarySearchTree a) -> Int -> (BinarySearchTree a)
insert NIL node = Node NIL node NIL
insert (Node  left current right) node 
 | current == node = Node left current right
 | current < node = Node left current (insert right node)
 | current > node = Node (insert left node) current right

delete :: (BinarySearchTree a) -> Int -> (BinarySearchTree a)
delete NIL _ = NIL
delete (Node left current right) x  
 | x == current = deleteX (Node left current right)
 | x  < current = Node (delete right x) current right
 | x  > current = Node left current (delete right x)

deleteX :: (BinarySearchTree a) -> (BinarySearchTree a)
deleteX (Node NIL current NIL) = NIL
deleteX (Node NIL current right) = right
deleteX (Node left current NIL) = left
deleteX (Node left current right) = Node left (successor (Node left current right)) (delete right (successor (Node left current right)))

height :: (BinarySearchTree a) -> Int
height NIL = 0
height (Node left current right) = 1 + max (height left) (height right)

--Ordenação

inOrder :: (BinarySearchTree a) -> [Int]
inOrder NIL = []
inOrder (Node left current right) = inOrder left ++ [current] ++ inOrder right

preOrder :: (BinarySearchTree a) -> [Int]
preOrder NIL = []
preOrder (Node left current right) = [current] ++ preOrder left ++ preOrder right

posOrder :: (BinarySearchTree a) -> [Int]
posOrder NIL = []
posOrder (Node left current right) = posOrder left ++ posOrder right ++ [current]

--Mínimo e máximo

minimo :: (BinarySearchTree a) -> Int
minimo (Node NIL current NIL) = current
minimo (Node left current right) = minimo (left)

maximo :: (BinarySearchTree a) -> Int
maximo (Node NIL current NIL) = current
maximo (Node left current right) = maximo (right)

--Sucessor e Predecessor

predecessor :: (BinarySearchTree a) -> Int
predecessor (Node left a right) = maximo left

successor :: (BinarySearchTree a) -> Int
successor (Node left a right) = minimo right 

btmin = head.inOrder

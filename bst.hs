module BST  where
data BinarySearchTree a = NIL | Node (BinarySearchTree a) a (BinarySearchTree a) deriving (Eq,Show)

empty NIL = True
empty _ = False

treeSize NIL = 0
treeSize (Node left current right) = 1 + treeSize left + treeSize right

--operações básicas 

search NIL _ = False
search (Node left current right) node
 | current == node = True
 | node < current = search left node
 | node > current = search right node 

insert NIL node = Node NIL node NIL
insert (Node  left current right) node 
 | current == node = Node left current right
 | current < node = Node left current (insert right node)
 | current > node = Node (insert left node) current right

delete NIL _ = NIL
delete (Node t1 v t2) x  
 | x == v = deleteX (Node t1 v t2)
 | x  < v = Node (delete t2 x) v t2
 | x  > v = Node t1 v (delete t2 x)

deleteX (Node NIL current NIL) = NIL
deleteX (Node NIL current right) = right
deleteX (Node left current NIL) = left
deleteX (Node left current right) = Node left (successor (Node left current right)) (delete right (successor (Node left current right)))

height NIL = 0
height (Node left current right) = 1 + max (height left) (height right)


--Ordenação

inOrder NIL = []
inOrder (Node left current right) = inOrder left ++ [current] ++ inOrder right

preOrder NIL = []
preOrder (Node left current right) = [current] ++ preOrder left ++ preOrder right

posOrder NIL = []
posOrder (Node left current right) = posOrder left ++ posOrder right ++ [current]

--Mínimo e máximo

minimo (Node NIL current NIL) = current
minimo (Node left current right) = minimo (left)

maximo (Node NIL current NIL) = current
maximo (Node left current right) = maximo (right)

--Sucessor e Predecessor

predecessor (Node left a right) = maximo left

successor (Node left a right) = minimo right 

main::IO()
main = do 
 let a = insert NIL 5
 let a2 = insert a 3
 let a3 = insert a2 8
 let a4 = insert a3 10
 let a6 = insert a4 110
 let b = maximo a6

 print b
 

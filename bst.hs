data BinarySearchTree a = NIL | Node (BinarySearchTree a) Int (BinarySearchTree a) deriving (Eq,Show)

empty NIL = True
empty _ = False

treeSize NIL = 0
treeSize (Node left a right) = 1 + treeSize left + treeSize right


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

deleteX (Node NIL v t2) = t2
deleteX (Node t1 v NIL) = t1
deleteX (Node t1 v t2) = (Node t1 v2 t2) --(delete t2 v2))
 where 
  v2 = leftistElement t2


leftistElement (Node NIL v _) = v
leftistElement (Node t1 _ _) = leftistElement t1

--Ordenação

inOrder NIL = []
inOrder (Node left current right) = inOrder left ++ [current] ++ inOrder right

preOrder NIL = []
preOrder (Node left current right) = [current] ++ preOrder left ++ preOrder right

posOrder NIL = []
posOrder (Node left current right) = posOrder left ++ posOrder right ++ [current]

main::IO()
main = do 
 let a = insert NIL 5
 let a2 = insert a 3
 let a3 = insert a2 8
 let b = posOrder a3
 print b

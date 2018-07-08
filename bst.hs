module BST where

data BinarySearchTree a = NIL | Node a (BinarySearchTree a) (BinarySearchTree a) deriving (Eq,Show)

{-verifica se a arvore está vazia-}
empty NIL = True
empty _ = False

{-verifica o tamanho da arvore-}
treeSize NIL = 0
treeSize (Node a left right) = 1 + treeSize left + treeSize right

{-arvore vazia, adiciona como raiz e só-}
insert NIL node = Node node NIL NIL
{--arvore nao vazia--}
insert (Node current left right) node | current == node = Node current left right
									  | current < node = Node current left (insert right node)
									  | current > node = Node current (insert left node) right








module AVL where 
import qualified BST as BST

--Rotações
balanceFactor :: (BST.BinarySearchTree a) -> (BST.BinarySearchTree a) -> Int
balanceFactor left right = (BST.height left) - (BST.height right)

balanceLL :: (BST.BinarySearchTree a) -> (BST.BinarySearchTree a)
balanceLL (BST.Node (BST.Node leftLeftSon leftSon leftRightSon) node rightSon) = (BST.Node leftLeftSon leftSon (BST.Node leftRightSon node rightSon))

balanceLR :: (BST.BinarySearchTree a) -> (BST.BinarySearchTree a)
balanceLR (BST.Node (BST.Node leftLeftSon leftSon (BST.Node lrlSon leftRightSon lrrSon)) node rightSon) = (BST.Node (BST.Node leftLeftSon leftSon lrlSon) leftRightSon (BST.Node lrrSon node rightSon))

balanceRL :: (BST.BinarySearchTree a) -> (BST.BinarySearchTree a)
balanceRL (BST.Node leftSon node (BST.Node (BST.Node rllSon rightLeftSon rlrSon) rightSon rightRightSon)) = (BST.Node (BST.Node leftSon node rllSon) rightLeftSon (BST.Node rlrSon rightSon rightRightSon)) 

balanceRR :: (BST.BinarySearchTree a) -> (BST.BinarySearchTree a)
balanceRR (BST.Node leftSon node (BST.Node rightLeftSon rightSon rightRightSon)) = (BST.Node (BST.Node leftSon node rightLeftSon) rightSon rightRightSon)

--controle
leftNode :: (BST.BinarySearchTree a) -> (BST.BinarySearchTree a)
leftNode (BST.Node left _ _) = left

rightNode :: (BST.BinarySearchTree a) -> (BST.BinarySearchTree a)
rightNode (BST.Node _ _ right) = right

currentNode (BST.Node _ current _) = current

-- operações básicas
insert :: (BST.BinarySearchTree a) -> Int -> (BST.BinarySearchTree a)
-- adicionando o primeiro no
insert BST.NIL node = (BST.Node BST.NIL node BST.NIL)
insert (BST.Node left current right) node
	--se o no a ser adicionado for igual a no atual
    | node == current = (BST.Node left current right)
    -- voce considera q o no esta desbalanceado quando ele esta igual a 2 ou a -2, pq entre o intervalo [-1,1] ela esta balanceada
    -- se for igual a 2 o respectivo no tem uma sub-arvore da esquerda maior que a da direita, analogamente para o -2
    -- verifica se o no que quero adicionar eh menor q o no atual, através do fator de balanceamento vê se está desbalanceada e para qual lado
    -- e em seguida verifica se no q vai adicionar vai ser um LeftLeftSon ou LeftRightSon
    | node < current && (balanceFactor leftInsertion right) ==  2 && node < currentNode left = balanceLL (BST.Node leftInsertion current right)
    | node < current && (balanceFactor leftInsertion right) ==  2 && node > currentNode left = balanceLR (BST.Node leftInsertion current right)
    -- verifica se o no que quero adicionar eh menor q o no atual, através do fator de balanceamento vê se está desbalanceada e para qual lado
    -- e em seguida verifica se no q vai adicionar vai ser um RightLeftSon ou RightRightSon
    | node > current && (balanceFactor left rightInsertion) == -2 && node < currentNode right = balanceRL (BST.Node left current rightInsertion)
    | node > current && (balanceFactor left rightInsertion) == -2 && node > currentNode right = balanceRR (BST.Node left current rightInsertion)
    -- se a arvore  estiver desbalanceada apenas adiciona ou a esquerda ou a direita
    | node < current  = (BST.Node leftInsertion current right)
    | node > current  = (BST.Node left current rightInsertion)
        where leftInsertion = insert left node
              rightInsertion = insert right node
              
delete :: (BST.BinarySearchTree a) -> Int -> (BST.BinarySearchTree a)
delete BST.NIL x = BST.NIL
-- deletando o unico no que tenho na arvore
delete (BST.Node BST.NIL node BST.NIL) x = if node == x then BST.NIL else (BST.Node BST.NIL node BST.NIL)
-- deletando o unico no a esquerda que arvore tem
delete (BST.Node left node BST.NIL) x = if node == x then left else (BST.Node (delete left x) node BST.NIL)
-- deletando o unico no a direita que a arvore tem 
delete (BST.Node BST.NIL node right) x = if node == x then right else (BST.Node BST.NIL node (delete right x)) 
delete (BST.Node left node right) x
	-- deletar a raiz, ai procura o antecessor do no que vai ser eliminado e coloca ele, fazendo as seguintes rotações
    | node == x = (BST.Node maxLeftDeletion minLeft right)
    -- verifica para qual lado vai na arvore e se ela com o delete vai ficar desbalanceado, se nao for ficar apenas deleta.
    | node > x && abs (balanceFactor leftDeletion right) < 2 = (BST.Node leftDeletion node right)
    | node < x && abs (balanceFactor left rightDeletion) < 2 = (BST.Node left node rightDeletion) 
    -- necessita de alguma rotacao simples (left ou right), pois nao esta totalmente balanceada.
    | node > x && (balanceFactor (leftNode right) (rightNode right)) < 0 = balanceRR (BST.Node leftDeletion node right)
    | node < x && (balanceFactor (leftNode left) (rightNode left)) > 0 = balanceLL (BST.Node left node rightDeletion)
    -- apenas deleta e faz o balanceamento se necessario, como n entrou no caso acima, as rotacoes que necessita aqui sao duplas. 
    | node > x = balanceRL (BST.Node leftDeletion node right)
    | node < x = balanceLR (BST.Node left node rightDeletion)
        where maxLeftDeletion = delete left minLeft
              leftDeletion = delete left x
              rightDeletion = delete right x
              minLeft = BST.maximo left


module AVL where 
import qualified BST as BST

balanceFactor left right = (BST.height left) - (BST.height right)

balanceLL (BST.Node (BST.Node leftLeftSon leftSon leftRightSon) node rightSon) = (BST.Node leftLeftSon leftSon (BST.Node leftRightSon node rightSon))
balanceLR (BST.Node (BST.Node leftLeftSon leftSon (BST.Node lrlSon leftRightSon lrrSon)) node rightSon) = (BST.Node (BST.Node leftLeftSon leftSon lrlSon) leftRightSon (BST.Node lrrSon node rightSon))
balanceRL (BST.Node leftSon node (BST.Node (BST.Node rllSon rightLeftSon rlrSon) rightSon rightRightSon)) = (BST.Node (BST.Node leftSon node rllSon) rightLeftSon (BST.Node rlrSon rightSon rightRightSon)) 
balanceRR (BST.Node leftSon node (BST.Node rightLeftSon rightSon rightRightSon)) = (BST.Node (BST.Node leftSon node rightLeftSon) rightSon rightRightSon)

insert BST.NIL node = (BST.Node BST.NIL node BST.NIL)
insert (BST.Node left current right) node
    | node == current = (BST.Node left current right)
    | node < current && (balanceFactor leftInsertion right) ==  2 = balanceLL (BST.Node leftInsertion current right)
    | node < current && (balanceFactor leftInsertion right) ==  2 = balanceLR (BST.Node leftInsertion current right)
    | node > current && (balanceFactor left rightInsertion) == -2 = balanceRL (BST.Node left current rightInsertion)
    | node > current && (balanceFactor left rightInsertion) == -2 = balanceRR (BST.Node left current rightInsertion)
    | node < current  = (BST.Node leftInsertion current right)
    | node > current  = (BST.Node left current rightInsertion)
        where leftInsertion = insert left node
              rightInsertion = insert right node

delete :: (BST.BinarySearchTree a) -> Int -> (BST.BinarySearchTree a)
delete BST.NIL x = BST.NIL
delete (BST.Node BST.NIL node BST.NIL) x = if node == x then BST.NIL else (BST.Node BST.NIL node BST.NIL)
delete (BST.Node left node BST.NIL) x = if node == x then left else (BST.Node left node BST.NIL)
delete (BST.Node BST.NIL node right) x = if node == x then right else (BST.Node BST.NIL node right)
delete (BST.Node left node right) x
    | node == x = (BST.Node left maxRight maxRightDeletion)
    | node > x && abs (balanceFactor leftDeletion right) < 2 = (BST.Node leftDeletion node right)
    | node < x && abs (balanceFactor left rightDeletion) < 2 = (BST.Node left node rightDeletion)
    | node > x && (balanceFactor (leftNode right) (rightNode right)) < 0 = balanceRR (BST.Node leftDeletion node right) 
    | node < x && (balanceFactor (leftNode left) (rightNode left)) > 0 = balanceLL (BST.Node left node rightDeletion)
    | node > x = balanceRL (BST.Node leftDeletion node right)
    | node < x = balanceLR (BST.Node left node rightDeletion)
        where maxRightDeletion = delete right maxRight
              leftDeletion = delete left x
              rightDeletion = delete right x
              maxRight = BST.maximo right

leftNode (BST.Node left _ _) = left
rightNode (BST.Node _ _ right) = right

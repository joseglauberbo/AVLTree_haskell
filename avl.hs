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

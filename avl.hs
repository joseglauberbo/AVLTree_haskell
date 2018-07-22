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

delete L d = L
delete (N v L L) d = if v == d then L else (N v L L)
delete (N v t L) d = if v == d then t else (N v t L)
delete (N v L u) d = if v == d then u else (N v L u)
delete (N v t u) d
    | v == d                            = (N mu t dmin)
    | v > d && abs (balFactor dt u) < 2 = (N v dt u)
    | v < d && abs (balFactor t du) < 2 = (N v t du)
    | v > d && (balFactor (left u) (right u)) < 0 = balanceRR (N v dt u) 
    | v < d && (balFactor (left t) (right t)) > 0 = balanceLL (N v t du)
    | v > d                                       = balanceRL (N v dt u)
    | v < d                                       = balanceLR (N v t du)
        where dmin = delete u mu
              dt   = delete t d
              du   = delete u d
              mu   = btmin u

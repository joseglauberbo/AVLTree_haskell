import BST

--rotacoes 

balanceFactor NIL = 0
balanceFactor (Node NIL current NIL) = 0
balanceFactor (Node left current right) = height left - height right

	


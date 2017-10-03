data BSTree : Type -> Type where
     Empty : Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right: BSTree elem) -> BSTree elem


insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                           LT => Node (insert x left) val (right)
                                           EQ => orig
                                           GT => Node (left) val (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Ord a => BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

-- data Tree elem  = Empty
--                 | Node (Tree elem) elem (Tree elem)

--
-- %name Tree tree1, tree2, tree3
--
-- insert : Ord elem => elem -> Tree elem -> Tree elem
-- insert x Empty = Node Empty x Empty
-- insert x orig@(Node left val right) = case compare x val of
--                                       LT => Node (insert x left) val (right)
--                                       EQ => orig
--                                       GT => Node (left) val (insert x right)

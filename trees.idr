
data Tree elem  = Empty
                | Node (Tree elem) elem (Tree elem)


%name Tree tree1, tree2, tree3

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val (right)
                                      EQ => orig
                                      GT => Node (left) val (insert x right)

Functor Tree where
  map func Empty = Empty
  map func (Node tree1 x tree2) = Node (map func tree1) (func x) (map func tree2)

Foldable Tree where
  foldr func init Empty = init
  foldr func init (Node tree1 x tree2)
    = let leftFold = foldr func init tree1
          rightFold = foldr func leftFold tree2 in func x rightFold

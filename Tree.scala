package binarytree {
    sealed trait Tree[+T] {
        /*
            P57 (**) Binary search trees (dictionaries).
                Write a function to add an element to a binary search tree.
                scala> End.addValue(2)
                res0: Node[Int] = T(2 . .)

                scala> res0.addValue(3)
                res1: Node[Int] = T(2 . T(3 . .))

                scala> res1.addValue(0)
                res2: Node[Int] = T(2 T(0 . .) T(3 . .))
                Hint: The abstract definition of addValue in Tree should be def addValue[U >: T <% Ordered[U]](x: U): Tree[U]. The >: T is because addValue's parameters need to be contravariant in T. (Conceptually, we're adding nodes above existing nodes. In order for the subnodes to be of type T or any subtype, the upper nodes must be of type T or any supertype.) The <% Ordered[U] allows us to use the < operator on the values in the tree.

                Use that function to construct a binary tree from a list of integers.

                scala> Tree.fromList(List(3, 2, 5, 7, 1))
                res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
                Finally, use that function to test your solution to P56.

                scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
                res4: Boolean = true

                scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
                res5: Boolean = false
        */
        def addValue[U >: T](value:U)(implicit ev: U => Ordered[U]): Tree[U] = this match {
            case End => Node(value, End, End)
            case Node(v, left, right) => 
                if(value > v) {
                    Node(v, left, right.addValue(value))
                } else {
                    Node(v, left.addValue(value), right)
                }
        }
    }
    case class Node[+T](value:T, left:Tree[T], right: Tree[T]) extends Tree[T] {
        override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    }
    case object End extends Tree[Nothing] {
        override def toString = "."
    }

    object Node {
        def apply[T](value: T): Node[T] = Node(value, End, End)
    }

    object Tree {
        /*
            P55
             (**) Construct completely balanced binary trees.
                In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
                Define an object named Tree. Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions. The function should take as parameters the number of nodes and a single value to put in all of them.

                scala> Tree.cBalanced(4, "x")
                res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...

            Basically starting from the root, divide the tree into two sides, and keep dividing until the n is 0. Return the List(leaf node) to the tree. The return type of the 
            recursion will be a List of all the combination of the tree. It will start from the leaf of the tree, like the left and right. Then, as the recursion returns, it will
            add the tree back into the list, by setting a new parent from both recursive function the left and the right side.
        */
        def cBalanced[T](num:Int, value:T): List[Tree[T]] = num match {
            case n if n < 1 => List(End)
            case n if n % 2 == 1 =>  // odd number including parent means both subtree will be the same
                /*
                    Because if you have a total of 7 nodes in this binary tree. The left subtree and the right subtree will have 3 each - meaning it will
                    you divide 7 / 2 = 3 since the 1 node will be the parent. So we want to recursively get the total combination of the right and left subtree, which has a 
                    total of 3 nodes.
                */
                val subTrees: List[Tree[T]] = cBalanced((n-1)/2, value) // recursively calling n/2 to go to the lower level of the tree traversal
                // combination of both subtrees
                for {
                    l <- subTrees
                    r <- subTrees
                } yield Node(value, l, r)
            case n if n % 2 == 0 => // even meaning there is one subtree will have greater child as balance
                /*
                    Same concepts as the odd number. Since it is an even number, one subtree needs to be larger than the other. Therefore, we need to get the left and the right 
                    sub tree. If the total tree is 4, then one subtree will be 1 and the other will be 2. (Because the last 1 is at the parent). Recursively getting the total
                    possible balanced tree from the left and right ,and construct that left and right subtree (in combination of Node(x,left,right), Node(x, right, left)) to 
                    get all possible combination of the current layer in the List.

                    Note: (n-1)/2 will get the smaller subtree, because (4-1)/2 = 1 . Whereas, (4-1)/2 +1 will get the second half of that even number.
                */
                val lesserSubtree : List[Tree[T]] = cBalanced((n-1)/2, value)
                val greaterSubtree: List[Tree[T]] = cBalanced((n-1)/2 + 1, value)
                lesserSubtree.flatMap {l =>
                    greaterSubtree.flatMap{g => 
                        List(Node(value, l,g), Node(value, g,l))
                    }
                }
        }

        def fromList[T](list:List[T])(implicit ev: T => Ordered[T]): Tree[T] = list.foldLeft[Tree[T]](End){(b,a) => b.addValue(a)}


        /*
        P58 (**) Generate-and-test paradigm.
            Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
            scala> Tree.symmetricBalancedTrees(5, "x")
            res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
        */
        def symmetricBalancedTrees[T](num:Int, value:T): List[Tree[T]] = cBalanced(num,value).filter(_.isSymmetric)

        
        
        
        implicit class TreeOps[T](tree: Tree[T]) {
            /*
            P56 (**) Symmetric binary trees.
                Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another. 
                We are only interested in the structure, not in the contents of the nodes.
                scala> Node('a', Node('b'), Node('c')).isSymmetric
                res0: Boolean = true
            */
            def isSymmetric: Boolean = {
                def isSymmetricR(root:Tree[T], root2: Tree[T]): Boolean = (root, root2) match {
                    case (Node(_, l1, r1), Node(_, l2,r2)) => 
                        isSymmetricR(l1,r2) && isSymmetricR(r1, l2)
                    case (End, End) => true
                    case _ => false
                }

                isSymmetricR(tree, tree)
            }
        }
        

    }

}

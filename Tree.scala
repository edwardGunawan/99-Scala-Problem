package binarytree {
    sealed trait Tree[+T] {
        def nodeCount: Int
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
        override def nodeCount: Int = left.nodeCount + right.nodeCount + 1 // 1 is the current count
        override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    }
    case object End extends Tree[Nothing] {
        override def nodeCount: Int = 0
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

        /*
            P59 (**) Construct height-balanced binary trees.
                In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
                Write a method Tree.hbalTrees to construct height-balanced binary trees for a given height with a supplied value for the nodes. The function should generate all solutions.

                scala> Tree.hbalTrees(3, "x")
                res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...

                Recursively go down the height tree. When the height is 1, the List(Node(value)). When height is 0 or -1, it will be List(End). We get the full-height, and the short-height. Then,
                2 for loop to combine all full height (since height 2 is equal to height 1 node + 1 more node (root)), and another for loop to combine the short height (height-2) and full-height (height-1).
                Since the it means either left or right subtree has a shorter height and that shorter height will be no more than 1. Therefore, it also get that combination.
        */
        def hbalTrees[T](height:Int, value:T): List[Tree[T]] = height match {
            case h if h < 1 => List(End)
            case h if h == 1 => List(Node(value))
            case _ => 
                // getting the previous height (this can be either on the left or right doesn't matter, as long as later we will add 1 more node as its root to build the current height tree)
                val fullheight = hbalTrees(height-1, value)
                // getting the height that is 1 more shorter than the previous height, since there will be a case where the left subtree will have a height of 1 difference with the right subtree
                val shorterheight = hbalTrees(height-2, value)

                // constructing the case where the both height of the subtree are the same
                val listOfFullheightCombination = for {
                    leftSubTree <- fullheight
                    rightSubTree <- fullheight
                } yield Node(value, leftSubTree, rightSubTree)

                // constructing the case where the either left or right subtree has a difference height of 1
                val listOfShorterHeightLeftSideCombination = for {
                    leftSubTree <- fullheight
                    shorterSubTree <- shorterheight
                } yield Node(value, leftSubTree, shorterSubTree)

                val listOfShorterHeightRightSideCombination = for {
                    rightSubTree <- fullheight
                    shorterSubTree <- shorterheight
                } yield Node(value, shorterSubTree, rightSubTree)

                listOfFullheightCombination ::: listOfShorterHeightLeftSideCombination ::: listOfShorterHeightRightSideCombination
        }

        /*
            P60 
            (**) Construct height-balanced binary trees with a given number of nodes.
            Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain? Clearly, MaxN = 2H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minHbalNodes that takes a height and returns MinN.
            scala> minHbalNodes(3)
            res0: Int = 4
            On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a maxHbalHeight function.

            scala> maxHbalHeight(4)
            res1: Int = 3
            Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.

            scala> Tree.hbalTreesWithNodes(4, "x")
            res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
            Find out how many height-balanced trees exist for N = 15.
        */
        def minHbalNodes(height: Int): Int = height match {
            case 0 => 0
            case 1 => 1
            case 2 => 2
            case n =>  minHbalNodes(n-1) + minHbalNodes(n-2) + 1
        }

        def maxHbalNodes(height:Int): Int = Math.floor(Math.pow(2, height) -1).toInt


        /*
            The Heights of the hbal tree will be log(n) where n is the nodes
            or we can get all the stream from 1 ~ any height and get the last node where 
            the value of the node is more equal to the node that is in the argument.
        */
        def maxHbalHeight(node:Int): Int = LazyList.from(1).takeWhile(minHbalNodes(_) <= node).last

        /*
            Typically division by 2 will be a height above it, until when it is 1 the height is 1 and count all 
            the way up the stack by adding 1 a the return call.
        */
        def minHbalHeight(node:Int): Int = 
            // when the node is 0, the height is 0
            if(node == 0) 0 
            else  minHbalHeight(node/2) + 1 // node/2 will go one height lower, meaning we account for the current height by having +1

        /*
            We get the minimum height for the nodes, and the maximum amount of height of the nodes. On each iteration, we 
            construct the height balanced tree with that height. Then, we filter out the nodes that matches the nodes in the 
            arguments. The hbalTrees may encounter various combination of hbal trees with various nodes - as long as those nodes
            matches the height balanced property. Therefore a height of 2 will have 2 nodes and 3 nodes.
        */
        def hbalTreesWithNodes[T](nodes:Int, value:T): List[Tree[T]] = 
            (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList

        

        
        

        
        
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

            def isBalancedHeight: Boolean = {
                def countHeight(root:Tree[T]): Int = root match {
                    case End => 1
                    case Node(_, left, right) => 
                        val leftHeight = countHeight(left)
                        val rightHeight = countHeight(right)
                        Math.max(leftHeight, rightHeight) + 1
                }

                tree match {
                    case End => true
                    case Node(_, left, right) =>
                        val lHeight = countHeight(left)
                        val rHeight = countHeight(right)
                        if(Math.abs(lHeight-rHeight) > 1) false
                        else right.isBalancedHeight && left.isBalancedHeight
                }
            }

            /*
            P61 (*) Count the leaves of a binary tree.
                    A leaf is a node with no successors. Write a method leafCount to count them.
                    scala> Node('x', Node('x'), End).leafCount
                    res0: Int = 1
            */
            def leafCount: Int = tree match {
                case Node(_,End, End) => 1
                case Node(_, l, r) => l.leafCount + r.leafCount
                case End => 0
            }

            /*
                
            61A (*) Collect the leaves of a binary tree in a list.
                    A leaf is a node with no successors. Write a method leafList to collect them in a list.
                    scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
                    res0: List[Char] = List(b, d, e)
            */
            def leafList:List[T] = tree match {
                case Node(t, End, End) => List(t)
                case Node(_, l, r) => l.leafList ::: r.leafList
                case End => Nil
            }

            /*
            P62 (*) Collect the internal nodes of a binary tree in a list.
                    An internal node of a binary tree has either one or two non-empty successors. Write a method internalList to collect them in a list.
                    scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
                    res0: List[Char] = List(a, c)
            */
            def internalList: List[T] = tree match {
                case Node(t, End,End) => Nil
                case Node(t, l, r) => t :: l.internalList ::: r.internalList 
                case End => Nil
            }
        }
        

    }

}

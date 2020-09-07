package multiwaytree {

    case class MTree[+T](value: T, children:List[MTree[T]]) {

        // def this(value:T) = this(value, List())
        override def toString = s"${value}${children.map(_.toString).mkString("")}^"
        //s"M(${value.toString}{${children.map(_.toString).mkString(",")}})"

        /*
            P70C (*) Count the nodes of a multiway tree.
                    Write a method nodeCount which counts the nodes of a given multiway tree.
                    scala> MTree('a', List(MTree('f'))).nodeCount
                    res0: Int = 2
        */
        def nodeCount: Int = 1 + children.foldLeft(0)((b,a) => {
            // println(s"b ${b}, a ${a}")
            b + a.nodeCount 
        })

        /*
                P71 (*) Determine the internal path length of a tree.
                    We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. 
                    By this definition, the tree in the figure of problem P70 has an internal path length of 9. Write a method internalPathLength to return that sum.
                    scala> "afg^^c^bd^e^^^".internalPathLength
                    res0: Int = 9

                It is the same as counting the nodes starting from the top, and each layer we count the node for the children.
                Therefore, we basically also counting first and second path as we go down the Mtree. The path is equal to the node count coming from the root.
            */
        def internalPathLength:Int = children.foldLeft(0)((r,c) => r + c.nodeCount + c.internalPathLength)
        
        /*
            Getting all child node first, and then appending the value after
        */
        def postorder:List[T] =  children.foldLeft(List.empty[T])((b,a) => b ::: a.postorder) :+ value
        // children.flatMap(_.postOrder) ::: List(value)

        /*
            P73 (**) Lisp-like tree representation.
                There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language. In Lisp almost everything is a list.
                Our example tree would be represented in Lisp as (a (f g) c (b d e)). The following pictures give some more examples.



                Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', with the atoms separated by spaces. We can represent this syntax as a Scala String. Write a method lispyTree which constructs a "lispy string" from an MTree.

                scala> MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree
                res0: String = (a (b c))
                As a second, even more interesting, exercise try to write a method that takes a "lispy" string and turns it into a multiway tree.
                // if it is the last element then just return the value
        */
        def lispyTree: String = if(children == Nil) {
            s"${value}"
        } else {
            s"(${value} ${children.map(_.lispyTree).mkString(" ")})" 
        }
    }

    object MTree {
        def apply[T](value:T): MTree[T] = new MTree(value, List.empty[MTree[T]])

        def apply[T](value:T, children:List[MTree[T]]): MTree[T] = new MTree(value, children)


         /*
            P70 (**) Tree construction from a node string.
                We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.
                By this rule, the tree in the figure opposite is represented as:

                afg^^c^bd^e^^^
                Define the syntax of the string and write a function string2MTree to construct an MTree from a String. Make the function an implicit conversion from String. Write the reverse function, and make it the toString method of MTree.

                scala> MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
                res0: String = afg^^c^bd^e^^^

                Same with P67 where we start from the top, split the string into the top layer and multiple child string, and recursively do the same
                on the the bottom layer.
        */
        def string2MTree(s:String): MTree[Char] = {
            // getting the bound of the child string, returning the position of the end value so we can get the substring of it
            // if the position is equal to the nesting, that means it reaches the end
            def nextStringBound(pos:Int, nesting:Int): Int = if(nesting == 0) {
                pos 
            } else {
                // println(s"${s(pos)} - nesting  : ${if(s(pos) == '^') nesting - 1 else nesting + 1}")
                nextStringBound(pos+1, if(s(pos) == '^') nesting - 1 else nesting + 1)
            }
            
            /*
              afg^^c^bd^e^^^  
              split to:
              (fg^), (c), (bd^e^)

              string2MTree("fg^^"), string2MTree("c^"), string2MTree("bd^e^^"), string2MTree("^")

            */
            def splitChildString(pos:Int): List[String] =  if(pos >= s.length-1) {
                Nil
            } else {
                val end = nextStringBound(pos+1, 1) // because 1 is the pos position
                // println(s"end ${end} substring : ${s.substring(pos, end-1)}")
                s.substring(pos, end-1) :: splitChildString(end) // split all the childstring
            }

            // println(splitChildString(1))

            MTree(s(0), splitChildString(1).map(string2MTree(_)))
        }

        implicit class MTreeOps(a:String) {
            def internalPathLength: Int = MTree.string2MTree(a).internalPathLength


            /*
                P72 (*) Construct the postorder sequence of the tree nodes.
                    Write a method postorder which constructs the postorder sequence of the nodes of a multiway tree. The result should be a List.
                    scala> "afg^^c^bd^e^^^".postorder
                    res0: List[Char] = List(g, f, c, d, e, b, a)
            */
            def postorder: List[Char] = MTree.string2MTree(a).postorder
        }
    }

}

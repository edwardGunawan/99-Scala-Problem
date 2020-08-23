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
    }

}

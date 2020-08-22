package multiwaytree {

    case class MTree[+T](value: T, children:List[MTree[T]]) {

        // def this(value:T) = this(value, List())
        override def toString = s"M(${value.toString}{${children.map(_.toString).mkString(",")}})"

        /*
            P70C (*) Count the nodes of a multiway tree.
                    Write a method nodeCount which counts the nodes of a given multiway tree.
                    scala> MTree('a', List(MTree('f'))).nodeCount
                    res0: Int = 2
        */
        def nodeCount: Int = 1 + children.foldLeft(0)((b,a) => {
            println(s"b ${b}, a ${a}")
            b + a.nodeCount 
        })
    }

    object MTree {
        def apply[T](value:T): MTree[T] = new MTree(value, List.empty[MTree[T]])

        def apply[T](value:T, children:List[MTree[T]]): MTree[T] = new MTree(value, children)
    }

}

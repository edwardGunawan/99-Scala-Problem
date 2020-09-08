package graph {
    /*
        The representations we introduced so far are bound to our implementation and therefore well suited for automated processing, but their syntax is not very user-friendly. Typing the terms by hand is cumbersome and error-prone. 
        We can define a more compact and "human-friendly" notation as follows: A graph is represented by a string of terms of the type X or Y-Z separated by commas. The standalone terms stand for isolated nodes, the Y-Z terms describe edges. 
        If an X appears as an endpoint of an edge, it is automatically defined as a node.
    */
    abstract class GraphBase[T,U]{
        case class Edge(n1: Node, n2:Node, value:U) {
            def toTuple = (n1.value, n2.value, value)

            override def toString = value match {
                case () => n1.value + edgeSep + n2.value
                case v  => n1.value + edgeSep + n2.value + labelSep + v
            }
        }
        val edgeSep: String
        val labelSep: String = "/"

        case class Node(value:T) {
            var adj: List[Edge] = Nil
            // neighbors are all nodes adjacent to this node.
            def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
        }

        var nodes : Map[T, Node] = Map()
        var edges: List[Edge] = Nil

        // If the edge E connects N to another node, returns the other nodes,
        // otherwise returns None.
        def edgeTarget(e:Edge, n:Node): Option[Node]

        override def equals(o: Any) = o match {
            case g: GraphBase[_, _] => (nodes.keys.toSet - g.nodes.keys.toSet == Set.empty &&
                                 edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple).toSet == Set.empty)
            case _ => false
        }

        def addNode(value: T) = {
            val n = new Node(value)
            nodes = Map(value -> n) ++ nodes
            n
        }

        override def toString = {
            val (edgeStrs, unlinkedNodes) =
                edges.foldLeft((Nil: List[String], nodes.values.toList))((r, e) => (e.toString :: r._1, r._2.filter((n) => n != e.n1 && n != e.n2)))
                "[" + (unlinkedNodes.map(_.value.toString) ::: edgeStrs).mkString(", ") + "]"
        }
        def toTermForm: (List[T], List[(T, T, U)]) =
            (nodes.keys.toList, edges.map((e) => (e.n1.value, e.n2.value, e.value)))
        def toAdjacentForm: List[(T, List[(T, U)])] =
            nodes.values.toList.map((n) => (n.value, n.adj.map((e) =>
            (edgeTarget(e, n).get.value, e.value))))
        
    
    }

    class Graph[T,U] extends GraphBase[T, U] {
        val edgeSep: String = "-"

        override def equals(o: Any) = o match {
            case g : Graph[_, _] => super.equals(g)
            case _ => false
        }

        def edgeTarget(e:Edge, n: Node): Option[Node] = 
            if(e.n1 == n) Some(e.n2)
            else if(e.n2 == n) Some(e.n1)
            else None
        
        def addEdge(n1: T, n2: T, value: U) = {
            // because the nodes is already been created
            val e = new Edge(nodes(n1), nodes(n2), value)
            nodes(n1).adj = e :: nodes(n1).adj
            nodes(n2).adj = e :: nodes(n2).adj
            edges = e :: edges
        }


        override def toTermForm: (List[T], List[(T, T, U)]) = {
            (nodes.keys.toList, edges.map(_.toTuple))
        }

        override def toAdjacentForm: List[(T, List[(T, U)])] = {
            nodes.map{
                case (t, node) => (t, edges.filter(_.n1.value == t).map(n => (n.n2.value, n.value)))
            }.toList
        }

        /*
            P82 (*) Cycle from a given node.
                Write a method named findCycles to find closed paths (cycles) starting at a given node in a graph. The method should return all cycles.
                scala> Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f")
                res0: List[List[String]] = List(List(f, c, b, f), List(f, b, c, f))
        */
        def findCycles(src: T): List[List[T]] = {
            def getNode(src: T): Option[Node] = nodes.get(src)

            def cycle(currNode:Node, visited:Set[T], path: List[T]): List[List[T]] = if(visited.contains(src)) {
                println(s"path ${path}")
                List(path.reverse) 
            } 
            else {
                currNode.neighbors.filterNot(n => {
                    // println(s"checking for visited ${visited} ${visited.contains(n.value} ${n.value}")
                    visited.contains(n.value)
                }).flatMap(n => {
                    cycle(n, visited + n.value, n.value :: path)
                }).filterNot{ r => 
                    r.isEmpty || r.length <= 3
                }
            }

            getNode(src).fold(List.empty[List[T]])(n => cycle(n,Set.empty[T], List(src)))
        }
    }

    class Digraph[T,U] extends GraphBase[T, U] {
        val edgeSep: String = ">"
        override def equals(o: Any) = o match {
            case g : Digraph[_, _] => super.equals(g)
            case _ => false
        }

        def edgeTarget(e:Edge, n:Node): Option[Node] = if(e.n1 == n) Some(e.n2) else None

        def addArc(source:T , dest:T, value: U) = {
            val e = new Edge(nodes(source), nodes(dest), value)
            edges = e :: edges
            nodes(source).adj = e :: nodes(source).adj
        }

        override def toTermForm: (List[T], List[(T, T, U)]) = (nodes.keys.toList, edges.map(_.toTuple))
        override def toAdjacentForm: List[(T, List[(T, U)])] = {
            nodes.map{
                case (t, node) => 
                    (t, edges.filter(_.n1.value == t).map(n => (n.n2.value, n.value)))
            }.toList
        }

        /*
            P81 (**) Path from one node to another one.
            Write a method named findPaths to find acyclic paths from one node to another in a graph. The method should return all paths.
            scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q")
            res0: List[List[String]] = List(List(p, q), List(p, m, q))
                
            scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k")
            res1: List[List[String]] = List()
        */
        def findPaths(source:T, dest:T): List[List[T]] = {
            // find nodes
            def findNodes(source: T): Option[Node] = nodes.get(source)

            // dfs
            def dfs(src: Node): List[List[T]] = {
                if(src.value == dest) {
                    List(List(src.value))
                } else {
                    /*
                        Think of foldLeft like it is iterating through each of the neighbors in the node.
                        Then, declarative way of thinking is that each iteration, you do a dfs on it, and the 
                        result, we want to append all the value back to the destination. If the destination is 
                        empty, then we will not append it because the List itself will be empty, and 
                        we cannot do map on an empty list.
                    */
                    src.neighbors.foldLeft(List.empty[List[T]]){(b, a) => 
                            (dfs(a).map(lst => src.value :: lst))  ::: b
                    }
                }
            }

            (findNodes(source).map{n => 
                dfs(n)
            }).getOrElse(List.empty[List[T]])
        }
    }

    abstract class GraphObjBase {
        type GraphClass[T, U]
        def addLabel[T](edges: List[(T,T)]) = edges.map(v => (v._1, v._2, ()))
        def term[T](nodes:List[T], edges: List[(T,T)]) = termLabel(nodes, addLabel(edges))
        def termLabel[T,U](nodes:List[T], edges: List[(T, T, U)]): GraphClass[T,U]
        def addAdjacentLabel[T](nodes:List[(T, List[T])]) =  nodes.map(a => (a._1, a._2.map((_, ())))) 
        def adjacent[T](nodes: List[(T, List[T])]) = adjacentLabel(addAdjacentLabel(nodes))
        def adjacentLabel[T,U](nodes: List[(T, List[(T,U)])]): GraphClass[T,U]
        implicit class RemoveDuplicateOps[A](arr:Array[A]) {
            def distinct:List[A] = 
                arr.toList.foldLeft(List[A]()) {
                    case (acc, item) if acc.contains(item) => acc
                    case (acc, item) => item::acc
            }   
        }
        
        /*
            P80 (***) Conversions.
            Write methods to generate the graph-term and adjacency-list forms from a Graph. Write another method to output the human-friendly form for a graph. Make it the toString method for Graph. Write more functions to create graphs from strings.
            Hint: You might need separate functions for labeled and unlabeled graphs.

            scala> Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm
            res0: (List[String], List[(String, String, Unit)]) = (List(d, k, h, c, f, g, b),List((h,g,()), (k,f,()), (f,b,()), (g,h,()), (f,c,()), (b,c,())))

            scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm
            res1: List[(String, List[(String, Int)])] = List((m,List((q,7))), (p,List((m,5), (q,9))), (k,List()), (q,List()))
        */
       val edgeSep: String
       val labelSep: String = "/"


        def fromStringBase[U, V](s: String)(mkGraph: (List[String], List[V]) => GraphClass[String, U], mkDigraph: (List[String], List[V]) => GraphClass[String, U])(parseEdge: String => V): GraphClass[String, U] = {
            assert(s(0) == '[')
            assert(s(s.length - 1) == ']')
            val tokens = s.substring(1, s.length - 1).split(", *").toList
            val nodes = tokens.flatMap(_.replaceAll("/.*", "").split("[->]")).distinct
            val edges = tokens.filter(_.matches(".*[->].*")).map(parseEdge)
            tokens.find(_.matches(".*>.*")) match {
                case None    => mkGraph(nodes, edges)
                case Some(_) => mkDigraph(nodes, edges)
            }
        }
        def fromString(s: String): GraphClass[String, Unit] =
            fromStringBase(s)(term[String], term[String]) { t =>
                val split = t.split("[->]")
                (split(0), split(1))
            }
        def fromStringLabel(s: String): GraphClass[String, Int] = 
            fromStringBase(s)(termLabel[String, Int], termLabel[String, Int]) { t =>
                val split = t.split("[->]")
                val split2 = split(1).split("/")
                (split(0), split2(0), split2(1).toInt)
            }
    }

    object Graph extends GraphObjBase {
        val edgeSep: String = "-"
        
        type GraphClass[T, U] = Graph[T, U]

        def termLabel[T,U](nodes: List[T], edges: List[(T, T , U)]) = {
            val g= new Graph[T,U]
            nodes.map(g.addNode)
            edges.map(v => g.addEdge(v._1, v._2, v._3))
            g
        }
        def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
            val g = new Graph[T, U]
            for ((v, a) <- nodes) g.addNode(v)
            for ((n1, a) <- nodes; (n2, l) <- a) {
                if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
                g.addEdge(n1, n2, l)
            }
            g
        }

        



        
    }
    /*
        A representation of the Node from the graph.
    */
    object Digraph extends GraphObjBase {
        val edgeSep: String = ">"
        type GraphClass[T,U] = Digraph[T, U]

        def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
            val g = new Digraph[T, U]
            nodes.map(g.addNode)
            edges.map(v => g.addArc(v._1, v._2, v._3))
            g
        }
        def adjacentLabel[T, U](nodes:List[(T, List[(T, U)])]) = {
            val g = new Digraph[T, U]
            for((n,a) <- nodes) g.addNode(n)
            for((s,a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
            g
        }

        
    }

    
}

package graph {

    import scala.annotation.tailrec
    import scala.concurrent.duration.span
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
            def degree: Int = adj.length
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
        
             /*
            P81 (**) Path from one node to another one.
            Write a method named findPaths to find acyclic paths from one node to another in a graph. The method should return all paths.
            scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q")
            res0: List[List[String]] = List(List(p, q), List(p, m, q))
                
            scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k")
            res1: List[List[String]] = List()
        */
        def findPaths(source:T, dest:T): List[List[T]] =  {
            
            def findPathsR(currNodes: Node, currPath: List[T]): List[List[T]] = {
                if(currNodes.value == dest) List(currPath)
                else {
                    // loop through the neighbors
                    // filter out the existing ndoes in the currPath, those are cycles
                    // append the neighboring nodes to the currPath, and do dfs again on those
                    currNodes.neighbors.filterNot(n => currPath.contains(n.value)).flatMap(n => findPathsR(n, n.value :: currPath))
                }
            }

            // get the source node from the maps of nodes
            // reverse the elements because we are appending from the beginning
            findPathsR(nodes(source), List(source)).map(_.reverse)
        }
        
        // This works for Digraph but will create Stack overflow for non-digraph
        // {
        //     // find nodes
        //     def findNodes(source: T): Option[Node] = nodes.get(source)

        //     // dfs
        //     def dfs(src: Node): List[List[T]] = {
        //         if(src.value == dest) {
        //             List(List(src.value))
        //         } else {
        //             /*
        //                 Think of foldLeft like it is iterating through each of the neighbors in the node.
        //                 Then, declarative way of thinking is that each iteration, you do a dfs on it, and the 
        //                 result, we want to append all the value back to the destination. If the destination is 
        //                 empty, then we will not append it because the List itself will be empty, and 
        //                 we cannot do map on an empty list.
        //             */
        //             src.neighbors.foldLeft(List.empty[List[T]]){(b, a) => 
        //                     (dfs(a).map(lst => src.value :: lst))  ::: b
        //             }
        //         }
        //     }

        //     (findNodes(source).map{n => 
        //         dfs(n)
        //     }).getOrElse(List.empty[List[T]])
        // }

        /*
            P85 (**) Graph isomorphism.
            Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 → N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.
            Write a method that determines whether two graphs are isomorphic.

            scala> Graph.fromString("[a-b]").isIsomorphicTo(Graph.fromString("[5-7]"))
            res0: Boolean = true

            Bijection function is one-to-one correspondence, or invertible function, is a function between the elemnts of two sets, where
            each elemnt of one set is paired with exactly one element of the other set, and each element of the other set is paired with 
            exactly one elment of the first set. There are no unpaired elements.

            Two graphs G1 and G2 are said to be isomorphic if - 
            - Their number of components (vertices and edges) are same
            - Their edge connectivity is retained
            Note - in short, out of the two isomorphic graphs, one is tweaked version of the other. An 
            unlabelled graph also can be thought of as an isomorphic graph.


        */
        def isIsomorphicTo[R,S](o: Graph[R, S]): Boolean = {
            // here we are getting all the possible mapping, therefore, the filter isValidMapping will filter out the ones that is 
            // in the iso that is not right.
            def listMapping(tNodes: List[Node], oNodes: List[o.Node]) = 
            for {
                tn <- tNodes
                on <- oNodes
            } yield (tn, on)

            
            /*
                Pre-check on the iso mapping neighbors to see if any of the nodes that is already map in
                iso have the same mapping.
            */
            def isValidMapping(iso: Map[Node, o.Node]): Boolean = nodes.values.forall{ tn =>
                // if the node doesn't exist in the iso map, then don't evaluate the mapping
                // if the node exist in the map then evaluate to check if all tn neighbors that contains in iso mapping is correct
                // n2 neighbors also contains node mapping of iso(neigh)
                // Get all the neighbors that contains in the iso mapping
                // check if the mapping between those neighbors is correct
                (!iso.contains(tn)) || tn.neighbors.filter(n => iso.contains(n)).forall{ neigh =>
                    iso(tn).neighbors.contains(iso(neigh))
                }
            }

            def isValidCompleteMapping(iso: Map[Node, o.Node]): Boolean = nodes.values.forall{tn => 
                // check the one to one mapping of the neighbors of nodes corresponding to tn 
                // is equal to the neighbors of iso(tn)
                Set(tn.neighbors.map(n => iso(n)): _*) == Set(iso(tn).neighbors: _*)
            }

            def isIsomorphicToR(gNodes: List[Node], oNodes: List[o.Node], iso:Map[Node,o.Node]): Boolean = {
                if(gNodes == Nil) isValidCompleteMapping(iso)
                else {
                    // filter our the invalid isomorphic first with isValidMapping
                    // then check if one combination exist, that means the graph is isomorphic
                    listMapping(gNodes, oNodes).filter{tup => isValidMapping(iso + tup)}.exists{tup =>
                        val (thisNode, thatNode) = tup
                        isIsomorphicToR(gNodes.filter(_ == thisNode), oNodes.filter(_ == thatNode), iso + tup)
                    }
                }
            }

            isIsomorphicToR(nodes.values.toList, o.nodes.values.toList, Map())
        }


        /*
        P86 (**) Node degree and graph coloration.
            a) Write a method Node.degree that determines the degree of a given node.
            scala> Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("a").degree
            res0: Int = 3
            b) Write a method that lists all nodes of a graph sorted according to decreasing degree.

            scala> Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree
            res1: List[Graph[String,Unit]#Node] = List(Node(a), Node(c), Node(b), Node(d))
            c) Use Welsh-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors. Make a method colorNodes that returns a list of tuples, each of which contains a node and an integer representing its color.

            scala> Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes
            res2: List[(Graph[String,Unit]#Node,Int)] = List((Node(a),1), (Node(b),2), (Node(c), 3), (Node(d), 2))
        */
        def nodesByDegree: List[Node] = nodes.values.toList.sortWith{
            case (n1, n2) => n1.degree > n2.degree
        }

        /*
            Welsh Powell Graph Colouring Algorihm
            labelling each vertex such that no two adjacent vertex have same color.
            This algorithm is also help to find the chromatic number of graph.

            1. Find the degree of each vertex
            2. List the vertices in order of descending degrees
            3. Colour the first vertex with color 1
            4. Move down the list and color all the vertices not connected to the coloured vertex, with the same color.
            5. Repeat step 4 on all uncolored vertices with a new color, in descnding order of degrees until all the 
            vertices are coloured.
        */
        def colorNodes: List[(Node, Int)] = {
            /*
                Step 4 color all vertices not connected to current vertices with the same color
            */
            @tailrec
            def colorVertices(color:Int, rest:List[Node], coloredSoFar: List[(Node,Int)]): List[(Node, Int)] = rest match {
                case Nil => coloredSoFar
                case head :: tl => 
                    val (neigh, nonNeigh) = tl.partition(n => isNeighbor(head, n))
                    colorVertices(color+1, neigh, (head, color) :: colorAllVertices(color, nonNeigh) ::: coloredSoFar)
            }

            def isNeighbor(n1: Node, n2: Node): Boolean = n1.neighbors.contains(n2)

            def colorAllVertices(color:Int, lst:List[Node]): List[(Node, Int)] = lst.map(n => (n, color))
            
            colorVertices(1, this.nodesByDegree, List.empty[(Node,Int)])
        }

        /*
            P87 (**) Depth-first order graph traversal.
            Write a method that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).
            scala> Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom("d")
            res0: List[String] = List(c, b, a, d)
        */
        def nodesByDepthFrom(src: T): List[T] = {

            // this is where we do dfs on that node if it is not yet visited
            // we will also call this in the init call in nodesByDepthFrom
            def depthFrom(node:Node, seen:Set[T]): List[T] = {
                // this will call dfs by adding the current node first to the visited
                // it won't cause stackoverflow because eventually the set will keep populating
                // since dfs will call depthFrom and depthFrom will call dfs again.
                dfs(node.neighbors, seen + node.value) ::: List(node.value) // here we are appending the node because it is the fist that we visited
            }
            
            
            def dfs(neighbors:List[Node], visited: Set[T]): List[T] = 
                neighbors match {
                    case Nil => Nil
                    case head :: tl if(visited.contains(head.value)) => 
                        dfs(tl, visited)
                    case head :: tl => 
                        //  find the nodesByDepthForm of the head
                        val depthForm = depthFrom(head, visited)
                        // traverse through the nxt neighbor and appending all the traverse depthForm
                        depthForm ::: dfs(tl, visited ++ depthForm)
                }
            

            nodes.get(src).map(n => depthFrom(n, Set())).getOrElse(List.empty[T])
        }
    
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
        def findCycles(src: T): List[List[T]] =  {
            // loop through all the neighbors.
            // find the paths from the neighbors to itself
            // append the initial value of List[T] to the src so it will be src - neighbors - src
            // filter out the ones that has length of more than 3
            nodes(src).neighbors.flatMap(n => findPaths(n.value, src)).map(lst => src :: lst).filter(_.length > 3)
        }
        
        // My implementation
        // {
        //     def getNode(src: T): Option[Node] = nodes.get(src)

        //     def cycle(currNode:Node, visited:Set[T], path: List[T]): List[List[T]] = if(visited.contains(src)) {
        //         println(s"path ${path}")
        //         List(path.reverse) 
        //     } 
        //     else {
        //         currNode.neighbors.filterNot(n => {
        //             // println(s"checking for visited ${visited} ${visited.contains(n.value} ${n.value}")
        //             visited.contains(n.value)
        //         }).flatMap(n => {
        //             cycle(n, visited + n.value, n.value :: path)
        //         }).filterNot{ r => 
        //             r.isEmpty || r.length <= 3
        //         }
        //     }

        //     getNode(src).fold(List.empty[List[T]])(n => cycle(n,Set.empty[T], List(src)))
        // }


        /*
            P83 (**) Construct all spanning trees.
                Write a method spanningTrees to construct all spanning trees of a given graph. With this method, find out how many spanning trees there are for the graph depicted to the right. The data of this example graph can be found below. When you have a correct solution for the spanningTrees method, use it to define two other useful methods: isTree and isConnected. Both are five-minute tasks!
                Graph:

                Graph.term(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
                        List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
                                ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
                                ('e', 'h'), ('f', 'g'), ('g', 'h')))
                scala> Graph.fromString("[a-b, b-c, a-c]").spanningTrees
                res0: List[Graph[String,Unit]] = List([a-b, b-c], [a-c, b-c], [a-b, a-c])
            
                Spanning tree is a subset of Graph G, which has all the vertices covered with minimum possible
                number of edges. Hence, a spanning tree does not have cycles and it cannot be disconnectd.

                Every connected and undirected Graph G has at least one spanning tree. A disconnected graph does not have any spanning
                tree, as it cannot be spanned to all its vertices.

                Spanning tree is basically used to find a minimum path to connect all nodes in a graph.
                - Civil network planning
                - Computer Network Routing Protocol
                - Cluster Analysis

                Minimum Spanning Tree
                In a weighted graph, a minimum spanning tree is a spanning tree that has minimnum weight than all
                other spanning trees of the same graph. In real-world situations, this weight can be measured as distance, congestion, traffic load 
                or any arbitrary value denoted to the edges.

                Minimum Spanning Tree Algorithm:
                - Kruskal's Algorithm
                - Prim's Algorithm
        */

    
  
        def spanningTrees: List[Graph[T, U]] = {
            // needs to get the List of nodes and the edges to construct the graph
            // loop through all the nodes, and do dfs until it hits all the nodes, store the edges while traverse
            def dfs(node: Node, edgeList: List[Edge], nodeSoFar: List[Node]): List[(List[Edge], List[Node])] = {
                // check if the neighbors of this nodes has all been visited. If the neighbors has all been visited that means 
                // we visited all the nodes (or these are the components in the graph that is connected)
                if(node.neighbors.foldLeft(true)((b,a) => b && nodeSoFar.contains(a))) {
                    List((edgeList, nodeSoFar))
                } else {
                    // map through the neighbors
                    // filter through the neighbors that is already visited
                    // add that edge and the visited node and recursively going through again
                    node.adj.filterNot{e => edgeList.contains(e) }
                    .flatMap{ e  => {
                        val neigh = edgeTarget(e, node).get
                        dfs(neigh, e :: edgeList, neigh :: nodeSoFar)
                    }}
                }
            }

        

            // find the nodes that is not yet traverse and travers through those nodes
            // we can just check the List[Node] after the DFS to input all the node to the node that has been traverse
            // we then can skip the already traverse node and go to the one that is not yet in the node to visit in foldLeft
            nodes.values.toList.foldLeft(List.empty[(List[Edge], List[Node])]){(b,a) => 

                    val res = dfs(a, List(), List(a))
                    // get the difference
                    val difference = res.filterNot{
                        case (edgeList, nodeLst) => 
                            // transform the List[List[Edge], List[Node]] => List[Set[Edge], Set[Node]]
                            // we know that the edge and node will be unique after running DFS.
                            b.map{tup => (tup._1.toSet, tup._2.toSet)}
                            .contains((edgeList.toSet, nodeLst.toSet))

                    }

                    b ++ difference
            }.map{
                case (edgeList, nodeLst) => Graph.termLabel(nodeLst.map(_.value), edgeList.map(_.toTuple))
            }
        }
        // We know if it is not a cycle and if it is a DAG if the total amount of graph is 1 (if only if there is no island in the graph)
        def isTree: Boolean = spanningTrees.length == 1

        // if the graph is connected, that means spanning trees has a length greater than 0 ( This is assuming if there is an island or other connected components it will return empty list)
        // in the answer on this question it will do that in order to check if the graph is connected.
        // spanningTrees.length > 0
        // However, in this case, we need to go through the spanning trees to check if all the list of graph has the same node length
        def isConnected: Boolean = (spanningTrees.length > 0) && spanningTrees.tail.foldLeft((true, spanningTrees.head.nodes.keys.toList.toSet)){(b,a) => 
            val (bool, set) = b

            // if the previous node has the same node value as the head of the set, AND the current not doesn't have any diff
            ((bool && set.diff(a.nodes.keys.toList.toSet).toList.length == 0), set)
        }._1


        /*
            P84 (**) Construct the minimal spanning tree.
                Write a method minimalSpanningTree to construct the minimal spanning tree of a given labeled graph. Hint: Use Prim's Algorithm. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found below.
                Graph:

                Graph.termLabel(
                List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
                    List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
                            ('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
                            ('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1)))
                scala> Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree
                res0: Graph[String,Int] = [a-b/1, b-c/2]

            Greedy algorithm to find the next edge that is the least amount of values through comparator.
            Having an edge list and a node list, remove the node list and the edge list one by one, and add them to the 
            treeEdges. At the end, construct the graph through all the nodes in the list.

            Prim's Algo : It is not a dfs. It is more like choosing the least amount of edges for all the nodes that has been touched.

            Nice video illustrating what Prims Algo is: https://www.youtube.com/watch?v=Uj47dxYPow8

        */
        def minimalSpanningTree(implicit f: (U) => Ordered[U]): Graph[T, U] = {
            // it will start from the tail so leaving the head as the src point. Therefore, when we get the adj value of the head
            // and loop through it, we will need to get it in terms of the edges.
            def minimalSpanningTreeR(edges: List[Edge], nodesLeft: List[Node], treeEdge: List[Edge]): Graph[T,U] = {
                // no more nodes to choose create the Graph
                if(nodesLeft.isEmpty) {
                    Graph.termLabel(nodes.keys.toList, treeEdge.map(_.toTuple))
                } else {
                    // the src will not be in nodesLeft so we get all the edges that is currently not in nodesLeft (that is included the previous nodes that we remove). Therefore, all the edges that we still have in the first Node.
                    // then afterwards, recursively calling the minimalSpanningTreeR we will remove the edges, leaving the 
                    // edges only the ones in the nodesLeft. Then while we moved one of the edges to treeEdge, we also get to remove
                    // the next traversed edge n2 from the nodesLeft.
                    // the value of this edge will be bigger and bigger, because there are other edges that we didn't pick
                    // the isNodeInEdgeNotInNodesLeft value prevent any cycles edges because we want to choose the edge that has one node in the nodesLeft .If both of them are not in nodesLeft we will not consider them.
                    val nextEdge = edges.filter(e => isNodeInEdgeNotInNodesLeft(e, nodesLeft)).reduceLeft((b,a) => if(b.value > a.value) a else b)
                    val newNodesLeft = nodesLeft.filter(n => edgeTarget(nextEdge, n) == None) // remove the nodes that is connected with the newEdge
                    
                    
                    val newEdges = edges.filterNot(e => e == nextEdge)
                    val newTreeEdge = nextEdge :: treeEdge
                    println(s"next Edge${nextEdge} new Nodes Left - ${newNodesLeft}")

                    minimalSpanningTreeR(newEdges, newNodesLeft, newTreeEdge)
                }
            }

            def isNodeInEdgeNotInNodesLeft(edge:Edge, nodesLeft: List[Node]): Boolean = !(nodesLeft.contains(edge.n1) && nodesLeft.contains(edge.n2)) // xor because one value needs to be in the edge to prevent cycles


            minimalSpanningTreeR(edges, nodes.values.toList.tail, Nil)
            
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

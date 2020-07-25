package logic {
    

    object S99Logic {
        implicit class S99LogicOps(a:Boolean) {
            /*
                
            P47 (*) Truth tables for logical expressions (2).
                Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit conversion from Boolean.) not will have to be left as a object method.
                scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
                A     B     result
                true  true  true
                true  false true
                false true  false
                false false false 
            */
            def and(b:Boolean): Boolean = S99Logic.and(a,b)
            def or(b:Boolean): Boolean = S99Logic.or(a,b)
            def xor(b: Boolean): Boolean = S99Logic.xor(a,b)
            def nand(b: Boolean): Boolean = S99Logic.nand(a,b)
            def nor(b:Boolean): Boolean = S99Logic.nor(a,b)
            def equ(b: Boolean): Boolean = S99Logic.equ(a,b)
            def impl(b:Boolean):Boolean = S99Logic.impl(a,b)
            

        }
        /*
        P46 (**) Truth tables for logical expressions.
            Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
            scala> and(true, true)
            res0: Boolean = true

            scala> xor(true. true)
            res1: Boolean = false
            A logical expression in two variables can then be written as an function of two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))

            Now, write a function called table2 which prints the truth table of a given logical expression in two variables.

            scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
            A     B     result
            true  true  true
            true  false true
            false true  false
            false false false
        */
        def and(a:Boolean , b :Boolean): Boolean = (a,b) match {
            case (true, true) => true
            case _ => false
        }
        def or(a:Boolean, b:Boolean): Boolean = (a,b) match {
            case (false ,false) => false
            case _ => true
        }
        def not(a:Boolean): Boolean = a match {
            case true => false
            case false => true
        }
        /*
            XOR: 
            Exclusive OR: 
            The true output results if one and only one of the input to the gate is true.
            Or Output true when input is different
            A B  A XOR B
            0 0     0
            0 1     1
            1 0     1
            1 1     0

            Algebra Expressions:  
                !((a || b) && (a && b))
                (a && !b) || (!a && b)
            
        */
        def xor(a:Boolean, b: Boolean): Boolean = not(equ(a,b))
        def nand(a:Boolean, b: Boolean): Boolean = not(and(a,b))
        def nor(a:Boolean, b:Boolean): Boolean = not(or(a,b))
        def equ(a:Boolean, b: Boolean): Boolean = or(and(a,b), and(not(a), not(b)))
        def impl(a:Boolean, b:Boolean):Boolean = or(not(a),b)

        def table2(func:(Boolean,Boolean) => Boolean): Unit = {
            println("A \t B \t result")
            for {
                a <- List(true,false)
                b <- List(true, false)
                res = func(a,b)
            } yield println(s"$a \t $b \t $res")
        }



        /*
        (**) Gray code.
            An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
            n = 1: C(1) = ("0", "1").
            n = 2: C(2) = ("00", "01", "11", "10").
            n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
            Find out the construction rules and write a function to generate Gray codes.

            scala> gray(3)
            res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
            See if you can use memoization to make the function more efficient.

            n-bit Gray code can be generated from n-1 bit Gray code with following steps:
            1. Let the list of (n-1) bit Gray code be L1. Create another L2 that is the reverse of L1.
            2. Modify L1 with "0" as its prefix
            3. Modify L2 with "1" as its prefix
            4. Concatenate both L1 and L2. The concatenated List is the required List of n Gray code.

            For example
            2 bit gray code: [00, 01, 11, 10]
            L1 : [00, 01, 11, 10] 
            L2 : [10, 11, 01, 00] (reverse of L1)
            concatenate "0" as L1 prefix - [000, 001, 011, 010]
            concatenate "1" as L2 prefix - [110, 111, 101, 100]
            Concatenate L1 and L2 = [000, 001, 011, 010, 110, 111, 101, 100]
        */ 

        def gray(num:Int): List[String] = {
            if(num == 0) List("")
            else {
                val L1 = gray(num-1)
                L1.map{ "0" + _} ::: L1.reverse.map { "1" + _}
            }
        }

        import scala.collection.mutable
        private val strings = mutable.Map(0 -> List(""))
        def grayMemoized(num:Int): List[String] = {
            if(!strings.contains(num)) {
                strings += (num -> (grayMemoized(num-1).map("0" + _) ::: grayMemoized(num-1).reverse.map("1" + _)))
            } 

            strings(num)
            
        }
        

        
    }
    
    /*
     P50 
     (***) Huffman code.
        First of all, consult a good book on discrete mathematics or algorithms
        for a detailed description of Huffman codes!
    
        We suppose a set of symbols with their frequencies, given as a list of
        (S, F) Tuples.  E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16),
        ("e", 9), ("f", 5)).  Our objective is to construct a list of (S, C)
        Tuples, where C is the Huffman code word for the symbol S.

        scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
        res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
    */
    object HuffmanCode {
        sealed trait Tree[A] {
            val freq: Int
            // create the coding for the prefix.
            // this will recursively going through the internal node until it hit the LeaftNode
            def toCode: List[(A, String)] = toCodePrefix("")
            /*
                Based on what value is, Internal Node or LeafNode, it will do the toCodePrefix differently.
            */
            def toCodePrefix(prefix:String): List[(A,String)]
        }
        case class InternalNode[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
            val freq: Int = left.freq + right.freq

            /*
                the internal node will combine the left prefix as 0 and right prefix as 1.
                It will recursively do that until it hits the LeafNode, which will be empty string.
                Therefore, the left will be 0 and the right will be 1. It will propagate upwards by keep appending the left and right value.

                Basically, it returns the left and right with the prefix to be determined, build up until it hits the base case.
                Ex: 
                    first layer - [(f,prefix + 0)] ::: [(d, prefix + 1)]
                    second layer - [(f, (prefix + 0)0), (d, (prefix + 0)1)] ::: [(e, prefix + 1)]
                    third layer - [(c,prefix + 0)] ::: [(b, prefix + 1)]
                    fourth layer - [(c, (prefix + 0)0), (b, (prefix + 0)1)] ::: [(f,((prefix + 1)00)), (d, (prefix +1)01), (e, (prefix+1)1)]
                    fifth layer - [(a, prefix + 0)] ::: [(c, (prefix + 1)00), (b, (prefix + 1)01), (f, (prefix + 1)100), (d, (prefix + 1)101), (e, (prefix + 1)11)]
                    last layer (toCode - toPrefix("")) - [(a, 0), (c,100), (b,101), (f, 1100), (d, 1101), (e, 111)]
            */
            override def toCodePrefix(prefix: String): List[(A, String)] = left.toCodePrefix(prefix + "0") ::: right.toCodePrefix(prefix + "1")
            
        }
        case class LeafNode[A](element:A, freq:Int) extends Tree[A] {
            override def toCodePrefix(prefix: String): List[(A, String)] = List((element, prefix))
        }

        def huffman[A](ls:List[(A,Int)]): List[(A,String)] = {
            import collection.immutable.Queue
            /*
                Deqeueue the smaller queue. If both queue have the same frequency, then dequeue the q1.
            */
            def dequeueSmallest(q1:Queue[Tree[A]], q2: Queue[Tree[A]]): (Tree[A], Queue[Tree[A]], Queue[Tree[A]]) = {
                if(q2.isEmpty) {
                    val (head, q1Tail) = q1.dequeue
                    (head, q1Tail, q2)
                } else if(q1.isEmpty || q2.front.freq < q1.front.freq) {
                    val (head, q2Tail) = q2.dequeue
                    (head, q1, q2Tail)
                } else {
                    val (head, q1Tail) = q1.dequeue
                    (head, q1Tail, q2)
                }
            }

            /*
                Recursively constructing the Huffman Tree code by using 2 queue.

                Dequeue twice to get the 2 smallest number, and add the frequency together to form the InternalNode, and enqueue again to q2, until
                the queue only have 1 value left.

            */
            def huffmanR(q1:Queue[Tree[A]], q2:Queue[Tree[A]]): List[(A,String)] = {
                if(q1.length + q2.length == 1) {
                    // the `toCode` will recursively calling the toPrefixCode that was generated in enqueue and dequeue and returns huffman code.
                    if(q1.isEmpty) q2.front.toCode
                    else q1.front.toCode
                }

                /*
                    dequeue the first 2 to find the smallest, and get q3 and q4 which is the new queue.
                    Dequeue again to get the second smallet.
                    Add both smallest value to form the Internal Node and enqueue it to the value second queue.
                */ 
                else {
                    val (val1, q3, q4) = dequeueSmallest(q1,q2)
                    val (val2, q5,q6) = dequeueSmallest(q3,q4)
                    val newInternalNode = InternalNode(val1, val2)
                    huffmanR(q5, q6.enqueue(newInternalNode))
                }
                
            }

            /*
                Create the Leaf node from the input List, and put it inside q1. q2 will be empty.
            */
            val ascendingOrderList = ls.sortWith{
                case ((a,intA), (b,intB)) => intA < intB
            }.map{
                case (a, freq) => LeafNode(a, freq)
            }

            val q1 = Queue.apply(ascendingOrderList: _*)
            huffmanR(q1, Queue.empty[Tree[A]])
        }
    }
}
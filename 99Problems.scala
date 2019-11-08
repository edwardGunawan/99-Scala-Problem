object Main {
    def main(args: Array[String]) = {
        val lst = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
        println(s"9.) ${pack(lst)}")
        println(s"10.) ${encoding(lst)}")
        println(s"11.) ${encodeModified(lst)}")
        println(s"12.) ${decode(encoding(lst))}")
        println(s"13.) ${encodeDirect(lst)}")
        println(s"14.) ${duplicate(List('a, 'b, 'c, 'c, 'd))}")
        println(s"15.) ${duplicateN(3,List('a, 'b, 'c, 'c, 'd))}")
        println(s"16.) ${drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")
        println(s"17.) ${split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")
        println(s"18.) ${slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")
        println(s"19. ${rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))})")
        println(s"19. ${rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))})")
        println(s"20. ${removeAt(0, List('a, 'b, 'c, 'd))}")
        println(s"21. ${insertAt("new", 1, List("a","b","c","d"))}")
    }

    // 9.) Pack consecutive duplicates of list elements into sublists.
    def pack[A](lst: List[A]): List[List[A]] = {
        if(lst.isEmpty) List(List())
        else {
            val (packed, rest) = lst.span(_ == lst.head)
            if (rest == Nil) List(packed)
            else packed :: pack(rest)
        }
    }

    // 10.) Run-length encoding of a list.
    def encoding[A](lst:List[A]): List[(Int,A)] = {
        pack(lst) map {l => (l.length, l.head)}
    }

    // 11. ) Modified run-length encoding.
    def encodeModified[A](lst: List[A]): List[Any] = {
        encoding(lst).map{tup => if(tup._1 == 1) tup._2 else tup}
    }

    // 12.) Decode a run-length encoded list
    def decode[A](lst: List[(Int, A)]) : List[A] = {
        lst flatMap {l => List.fill(l._1)(l._2)}
    }

    // 13.) Run-length encoding of a list (direct solution)
    def encodeDirect[A](lst: List[A]): List[(Int,A)] = {
        // pattern matching way
        // def encode(head:A, lst:List[A], count: Int): List[(Int,A)] = lst match {
        //     case Nil => List((count, head))
        //     case h :: rest if(h == head) => encode(head, rest, count+1)
        //     case list => (count, head) +: encode(list.head,list, 0)
        // }
        // encode(lst.head, lst, 0)

        // regular way
        if(lst.isEmpty) List()
        else {
            val (packed, rest) = lst span (_ == lst.head)
            (packed.length, packed.head) +: encodeDirect(rest)
        }
    }

    // 14.) Duplicate the element of list
    def duplicate[A](lst: List[A]) :List[A] = {
        // lst flatMap {l => List.fill(2)(l)}
        lst match {
            case h :: t => h :: h :: duplicate(t)
            case _ => Nil
        }
    }

    // 15.) Duplicate the elements of a list
    def duplicateN[A](n: Int, lst:List[A]) : List[A] ={
        lst flatMap { l => List.fill(n)(l)}
    }

    // 16.) Drop every Nth element from list
    def drop[A](n: Int, lst:List[A]): List[A] = {
        def dropRecursive(c:Int, currList:List[A]): List[A] = (c,currList) match {
            case (_, Nil) => Nil
            case (1, _ :: tail) => dropRecursive(n,tail)
            case (_, h:: tail) => h :: dropRecursive(c-1,tail)
        }
        dropRecursive(n, lst)
    }

    // 17.) Split list into two parts
    def split[A](n: Int, lst:List[A]): (List[A],List[A]) = {
        // built in
        // lst splitAt (n)

        // simple recursion
        (n, lst) match {
            case (_, Nil) => (Nil, Nil)
            case (0, lst) => (Nil, lst)
            case (n, h :: t) => {
                val (pre, post) = split(n-1, t)
                (h :: pre, post)
            }
        }
        
        // functional
        // (lst.take(n), lst.drop(n))
    }

    // 18.) Extract a slice from a list
    def slice[A](start:Int, end:Int, lst:List[A]): List[A] = {
        // builtin 
        // lst.slice(start,end)

        // their version of recursive
        // (start, end, lst) match {
        //     case (_,_, Nil) => Nil
        //     case (_,e,_) if e <= 0 => Nil
        //     case (s, e, h:: tail) if s <= 0 => h :: slice(0,e-1,tail)
        //     case (s,e,h::tail) => slice(s-1,e-1, tail)
        // }

        // functional
        lst drop start take (end - (start max 0))

        // my version of recursion
        // def helper(index: Int, lst:List[A]): List[A] = lst match {
        //     case h::tail if(index >= start && index < end)  => h :: helper(index + 1, tail)
        //     case h :: tail => helper(index +1, tail)
        //     case _ => Nil
        // }

        // helper(0, lst)
    }

    // 19.) Rotate the list N places to the left
    def rotate[A](n: Int, lst: List[A]): List[A] = {
        // this gets you the split point for rotation
        val nBounded = if (lst.isEmpty) 0 else n % lst.length

        // if it is negative just add the full length to the number
        if (nBounded < 0) rotate(nBounded + lst.length, lst)

        else {
            (lst drop nBounded) ::: (lst take nBounded)
        }
    }

    // 20.) Remove the kth element from the list
    def removeAt[A](n : Int,lst : List[A]) : (List[A], A) = {
        if(n < 0) throw new NoSuchElementException
        (n,lst) match {
            case (_,Nil) => throw new NoSuchElementException
            case (0, h::tail) => (tail, h)
            case (_, h::tail) => {
                // this is the method that they often use
                val (t, e) = removeAt(n-1, tail)
                (h::t, e)
            }
        }
    }

    //21.) Insert an element at a given position into a list.
    def insertAt[A](n:A, i:Int, lst:List[A]) : List[A] = {
        def helperInsert(currIndex:Int, lst:List[A]): List[A] = {
            lst match {
                case Nil => throw new NoSuchElementException
                case _ if i == currIndex => n :: lst
                case head :: tl => head :: helperInsert(currIndex +1, tl)
            }
        }

        helperInsert(0,lst)
    }
}
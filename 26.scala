object Problem26 extends App{
    /*
    Generate the combinations of K distinct objects chosen from the N elements of a list.
    In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
    Example:

    scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
    */
    // this is more understandable (this is the same as using the DFS way)
    def combinations[A](num:Int, lst: List[A]) : List[List[A]] = (num,lst) match {
        case (0, _) => List(Nil)
        case (_, Nil) => Nil
        // choose current value or not, but if you choose you need to append it from the front
        // in java the i increment always, and here there is no i, so it will always be tail
        case (n, head::tl) => combinations(n-1, tl).map(head :: _) ::: combinations(n,tl)
       

    }
    /*
    flatMapSublists is like list.flatMap, but instead of passing each element
    to the function, it passes successive sublists of L.
    So it become like this : List(1,2,3, 2,3, 3) flatMap into the successive sublist
    so it append the head to the initial of the List, becoming like this : 
    if n is 2
        List(1,2)  List(1,3)
        List(2,3)
    if n is 1
        List(1) -------- List(List(1,2,3))
        List(2) - - - List(List(2,3))
                 |
        List(3) -
        Passing the successive sublist of the function. This is where the meat of the algorithm is.
        You either choose the function or you don't choose the function.
    Once you go to the excluded function, on the initial value, that is when the pivot changes to 2,3
    */
    def flatMapSublists[A,B](ls:List[A])(f: (List[A]) => List[B]):List[B] = ls match {
        case Nil => Nil
        case sublist @ (_ :: tl) => {
            println(s"getting into included $sublist")
            val include = f(sublist) 
            println(s"getting into excluded $tl")
            val exclude = flatMapSublists(tl)(f)
            println(s"include : $include")
            println(s"exclude: $exclude")
            println(s"included excluded: ${include ::: exclude}")
            include ::: exclude
        }
    }
    
    def combinationExample[A](num:Int, ls:List[A]) : List[List[A]] = {
        if(num == 0) List(Nil)
        else {
            flatMapSublists(ls)(sl => {
                println(s"getting into flatMapSubLists in combination Example $num - ${sl.tail}")
                // if combinationExample(num-1,sl.tail) is an empty List, the map function will not run
                val whatIsThis = combinationExample(num-1, sl.tail).map{sl.head :: _} // this is where they append all the List(List(b), List(c)) with a because of the map. So essentially this can go O(n*n) as list gets long 
                println(s"what is this $whatIsThis")
                whatIsThis
            })
        }
    }

    println(combinationExample(3, List('a', 'b', 'c')))
    // val res = flatMapSublists(List(1,2,3))(sl => {
        // sl
    // })
    // println(res)
    // println(s"passing combinations of 3 : ${combination(3, List('a', 'b', 'c', 'd', 'e', 'f'))}")

    
}
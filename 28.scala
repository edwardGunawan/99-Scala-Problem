object Problem28 extends App{
    /*
    Sorting a list of lists according to length of sublists.
    a) We suppose that a list contains elements that are lists themselves. 
    The objective is to sort the elements of the list according to their length. 
    E.g. short lists first, longer lists later, or vice versa.
    Example:

    scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    b) Again, we suppose that a list contains elements that are lists themselves. But this time the 
    objective is to sort the elements according to their length frequency; i.e. 
    in the default, sorting is done ascendingly, lists with rare lengths are placed, 
    others with a more frequent length come later.

    Example:

    scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), 
    List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), 
    List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    Note that in the above example, the first two lists in the result have length 4 and 1 and 
    both lengths appear just once. The third and fourth lists have length 3 and there are two 
    list of this length. Finally, the last three lists have length 2. This is the most frequent 
    length.
*/

def lsort[A](lst:List[List[A]]): List[List[A]] = {
    lst.sortWith(sortByLength)
    // or 
    // lst.sort{_.length < _.length}
}

def sortByLength[A](lstA:List[A], lstB:List[A]): Boolean = {
    lstA.length < lstB.length
}

def pack[A](lst:List[A]):List[List[A]] = {
    if(lst == Nil) {
        List(Nil)
    } else {
        val (consecutive, rest) = lst.span(_ == lst.head)
        if(rest == Nil) List(consecutive)
        else consecutive :: pack(rest)
    }
}

def encode[A](lst:List[A]): List[(Int, A)] = {
    pack(lst).map{ res =>
        (res.length, res.head)
    }
}

/*
    Algorithm: 
    group all the length of frequency to a Map based on the length value (1,2,2,3,3,4) will be 
    ((1,1)(2,2),(2,3),(1,4)) key will be the frequency, and value will be the length of the list

    Then swap the key and the value so becoming (length -> frequency)

    sort the list based on the frequency Map so that frequency the least will first
*/
def lsortFreq[A](lst:List[List[A]]):List[List[A]] = {
    def swapTuple[A,B](tuple:(A,B)):(B,A) = (tuple._2,tuple._1)

    // you can technically replace encode with groupBy
    // val freqMap = res.map(_.length).sorted.groupBy(identity).view.mapValues(_.length).toMap

    // because encode is (res.length, res.head) instead of groupBy it is (res.head, res), in this case res.head will be the length of 
    // the List, res.length will be the frequency of how many the same length amount of list appear
    val freqMap = Map(encode(lst.map(_.length).sorted).map{ swapTuple(_) }: _*) // the last _* is to convert it to Map
    println(freqMap)

    // sort the list based on the frequenct in the Map
    lst.sortWith((e1,e2) => freqMap(e1.length) < freqMap(e2.length))
    
}




val res = lsort(List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h'), List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o')))
println(res.map(_.length).sorted.groupBy(identity).view.mapValues(_.length).toMap)

// print(pack(res))
// println(encode(res))
// println(res)
// val sortFreq = lsortFreq(List(List('a', 'b', 'c'), List('d', 'e'), 
//     List('f', 'g', 'h'), List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o')))
// println(sortFreq)

}
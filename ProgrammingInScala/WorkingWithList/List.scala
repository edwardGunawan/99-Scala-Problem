object Main {
    def main (args:Array[String]):Unit = {
        val l = List(1,3,4,5,2,3,55,7,11,0,-1)
        println(s"original list $l")
        println(isort(l))
        println(reverse(l))
    }


    def isort(xs:List[Int]):List[Int] = {
        if(xs.isEmpty) Nil
        else insert(xs.head, isort(xs.tail))
    }

    // insert the xs in the right position
    def insert(x:Int, xs:List[Int]):List[Int] = {
        if (xs.isEmpty || x <= xs.head) x :: xs
        else xs.head :: insert(x,xs.tail)
    }

    def isortWithPatternMatchin(xs:List[Int]): List[Int] = xs match {
        case Nil => Nil
        case head :: tl => insertWithPaternMatchin(head,isortWithPatternMatchin(tl))
    }

    def insertWithPaternMatchin(x:Int, xs:List[Int]) : List[Int] =  xs match {
        case Nil => Nil
        case head :: tl => if(x <= head) x :: xs else head :: insert(x,tl)
    }

    /*
        There are n recursive calls, and each calls needs to concatenate, so it will be O(n^2)
    */
    def reverse(xs:List[Int]): List[Int] = xs match {
        case Nil => xs
        case head :: tl => reverse(tl) ::: List(head)
    }
        
}
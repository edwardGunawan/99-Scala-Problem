object Problem22 {
    /*
        Create a list containing all integers within a given range.
        Example:
        scala> range(4, 9)
        res0: List[Int] = List(4, 5, 6, 7, 8, 9)
    */

    def main(args:Array[String]): Unit = {
        // regular
        println(s"regular recursive : ${range(4,9)}")
        println(s"${range(1,2)}")
        println(s"${range(12,55)}")

        // tail recursive
        println("Tail Recursive")
        println(rangeTailRecursive(4,9))
        println(rangeTailRecursive(1,2))
        println(rangeTailRecursive(10,50))

    }

    def range(low:Int, high:Int): List[Int] = 
        if(low == high) List(high) else low :: range(low+1, high)

    // tail recursive way
    def rangeTailRecursive(low:Int, high:Int):List[Int] = {
        def tailRecursive(high:Int, currList:List[Int]): List[Int] = {
            if(low == high) high :: currList  else tailRecursive(high-1, high :: currList) // you append from the back
        }
        tailRecursive(low,high,List.empty[Int])
    }

    // classic functional approach is to use `unfoldr`
    // the opposite of foldRight which is produce a List from a seed value
    // each function call returns a a result which is the element in the list
    // the list is done when the function returns None instead of Some(x)
    // adding an element to the head is constant operation but adding an element to the tail takes
    // O(n) time
    // The difference between unfoldLeft is to reverse starting from the Left said, so you will need 
    // to insert the value to the parameter while appending it
    def unfoldRight[A,B](s:B)(f: B => Option[A,B]): List[A] = f(s) match {
        case None => Nil
        // r is the current value, and n will be the next value that you will want to call the function off
        case Some((r,n)) => f :: unfoldRight(n)(f)
    }

    def rangeFunctional(low:Int, high:Int):List[Int] = {
        unfoldRight(low) { n =>
            if(n > high) None
            else Some((n, n+1))
        }
    }


}
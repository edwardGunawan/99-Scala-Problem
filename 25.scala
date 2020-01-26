import java.{util => ju}
object Problem25 {
    /*
     (*) Generate a random permutation of the elements of a list.
        Hint: Use the solution of problem P23.
        Example:

        scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
        res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
    */
    
    def randomPermute[A](lst:List[A]):List[A] = {
        randomSelect(lst.length, lst)
    }

    def randomSelect[A](num:Int, lst:List[A]):List[A] = {
        // getting kth element from list but incorporate random in it
        if(num == 0) Nil
        else {
            lst match {
                case Nil => throw new NoSuchElementException
                case _ => 
                    val (kthElement, rest) = getKthElement((new util.Random).nextInt(lst.length), lst)
                     kthElement :: randomSelect(num-1, rest)
            }
            
        }
    }

    def getKthElement[A](kth:Int, lst:List[A]):(A, List[A]) = {
        if(kth == 0) {
            (lst.head, lst.tail)
        } else {
            val (found, rest) = getKthElement(kth-1, lst.tail)
            (found, lst.head :: rest)
        }
    }

    def main(args:Array[String]):Unit = {
        println(randomPermute(List('a', 'b', 'c', 'd', 'e', 'f')))
    }
    
}
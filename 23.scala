import java.{util => ju}
object Problem23 {
    /*
    Extract a given number of randomly selected elements from a list.
    Example:
    scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    res0: List[Symbol] = List('e, 'd, 'a)
    */

    def randomSelect[A](numSelect:Int, lst:List[A]): List[A] = {
        if(numSelect <= 0) Nil
        else {
            val (rest, found) = removeAt((new util.Random).nextInt(lst.length), lst)
            // keep recursing through the next rest value until it numSelect is less than 0
            found :: randomSelect(numSelect-1, rest)
        }
        
    }

    def removeAt[A](elementToRemove:Int, lst:List[A]):(List[A], A) = lst match {
        case head :: tl => 
            if(elementToRemove == 0) (tl, head) 
            else {
                val (tail, found) = removeAt(elementToRemove-1, tl)
                (head :: tail, found)
            }
        case Nil => throw new NoSuchElementException
    
            

    }

    def main(args:Array[String]):Unit = {
        println(removeAt(1, List('a', 'b', 'c', 'd')))
        println(randomSelect(3, List('a', 'b', 'c', 'd', 'f', 'g', 'h')))
    }

}
object Problem24 {
 /*
    scala> lotto(6, 49)
    res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 */

 def lotto(n:Int, set:Int):List[Int] = {
     if(n < 0) Nil
     else {
         (new util.Random).nextInt(set) :: lotto(n-1, set)
     }
 }

 // because every time when you put the initial it goes through the function
 def unfold[A,B](initial:A)(func: A => Option[(B,A)]):List[B] = func(initial) match {
     case None => Nil
     case Some((curr, nextValue)) => curr :: unfold(nextValue)(func)
 }

 def lottoFunctional(n:Int, set:Int):List[Int] = {
     unfold(((new util.Random).nextInt(set), n)){ arg =>
        val (random, i) = arg
        if(i == 0) None
        else {
            Some(random, ((new util.Random).nextInt(set), i-1))
        }
    }
 }

 def main(string:Array[String]):Unit = {
    //  println(lotto(6, 49))
    println(lottoFunctional(6,49))
 }
}
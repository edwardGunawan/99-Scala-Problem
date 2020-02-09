package arithmetic {
    class S99Int(val start:Int) {
        import S99Int._

        /*
            P31 Determine whether a given integer number is prime.
                scala> 7.isPrime
                res0: Boolean = true
        solution approach:
            A number is prime if it is not divisible by any prime number less than or equal to 
            its squre root.

            Using stream to create lazy infinite of prime number.
            The mutual recursion between `prime` and `isPrime` works because of the limit on `isPrime`
            to the squre root of the number being tested.
        */
        // take all the stream in prime that is less than sqrt and check if that is a division of start
        // the primes w
        def isPrime:Boolean = (start > 1) && {
            // when it goes to primes it already computed the value within prime and return it as a prime number
            // Therefore, there will not have any infinite recursion involved, takeWhile will stop at the predicate below
            // Other non-prime usualy are consist of the prime number
            primes takeWhile{n => {
                n <= Math.sqrt(start)
            }} forall(start % _ != 0)

            /* my implementation
            if(start <= 1) {
                false 
            } else{
                // implementation to check if the number is divisible by that number, if it all has a remainder than simple return true
                (2 to start).toList.takeWhile(start % _ > 0).length == (start-2)
            }
            */
        }

     
    }

    object S99Int {
        implicit def int2S99Int(i:Int):S99Int = new S99Int(i)
        // create a infinite of prime number, because any even number can be divisible by 2 and thus it is not prime
        // therefore, only loop through odd numbers and filter all the prime number it has
        val primes = Stream.cons(2, Stream.from(3,2) filter(_.isPrime))
    }
}





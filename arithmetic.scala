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

        /*
            P33
            Determine whether two positive integer numbers are coprime.
            Two numbers are coprime if their greatest common divisor equals 1.
            scala> 35.isCoprimeTo(64)
            res0: Boolean = true
        */
        def isCoprimeTo(num:Int): Boolean = S99Int.gcd(start,num) == 1
        

     
    }

    object S99Int {
        implicit def int2S99Int(i:Int):S99Int = new S99Int(i)
        // create a infinite of prime number, because any even number can be divisible by 2 and thus it is not prime
        // therefore, only loop through odd numbers and filter all the prime number it has
        val primes = Stream.cons(2, Stream.from(3,2) filter(_.isPrime))

        /*
            P32
            Determine the greatest common divisor of two positive integer numbers.
            Use Euclid's algorithm.
            scala> gcd(36, 63)
            res0: Int = 9
        */
        // underlying algorithm states that the gcd will not change if the larger number
        // is replaced by the difference of the smaller number
        // for example: gcd(36,60) = (12 x 3 = 36, 12 x 5 = 60) is equal to the gcd of 24 = 12 x 2 and 36 = 12 x 3
        // if we use division, will stop until both numbers are the same (which is the remainder of 0)
        // because if a mod b is 0 then b is the GCD of gcd(a,b). Therefore, a,b will have remainder in that sense.
        def gcd(a:Int, b:Int) : Int = {
            if(a < b) {
                gcd(b,a)
            }
            else if(b == 0){
                a
            } else {
                gcd(b, a % b)
            }
        }
    }
}





package arithmetic {

    import java.time.Instant
    import scala.concurrent.Future
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

        /*
            P34
            Calculate Euler's totient function phi(m).
            Euler's so-called totient function phi(m) is defined as the 
            number of positive integers r (1 <= r <= m) that are coprime to m.
            scala> 10.totient
            res0: Int = 4
        */
        def totient: Int = (1 to start).foldLeft(0){(a,b) =>
            if(b.isCoprimeTo(start)) a+1
            else a
        }

        /*
            P35 
            Determine the prime factors of a given positive integer.
            Construct a flat list containing the prime factors in ascending order.
            scala> 315.primeFactors
            res0: List[Int] = List(3, 3, 5, 7)
        */
        def primeFactors:List[Int] = {
            def primeFactorsR(num:Int, p:Stream[Int]): List[Int] = 
                if(num.isPrime) List(num)
                else if(num % p.head == 0) p.head :: primeFactorsR(num / p.head, p)
                else primeFactorsR(num, p.tail)
            
            primeFactorsR(start,primes)
        }

        /*
            P36
            Determine the prime factors of a given positive integer (2).
            Construct a list containing the prime factors and their multiplicity.
            scala> 315.primeFactorMultiplicity
            res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
            Alternately, use a Map for the result.

            scala> 315.primeFactorMultiplicity
            res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
        */
        def primeFactorMultiplicity: List[(Int,Int)] = {
            start.primeFactors.groupBy(identity).mapValues(_.size).toList
        }

        /*
            P37
            Calculate Euler's totient function phi(m) (improved).
            See problem P34 for the definition of Euler's totient function. 
            If the list of the prime factors of a number m is known in the form of 
            problem P36 then the function phi(m>) can be efficiently calculated as follows: 
            Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their multiplicities) of a given number m. 
            Then phi(m) can be calculated with the following formula:
            phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...

            Note that ab stands for the bth power of a.
        */
        def totientImprov: Int = start.primeFactorMultiplicity.foldLeft(1){(acc,tup) =>
            val (p, m) = tup
            acc * ((p-1) * Math.pow((p),m-1)).toInt
        }

        /*
         P38
         (*) Compare the two methods of calculating Euler's totient function.
            Use the solutions of problems P34 and P37 to compare the algorithms. 
            Try to calculate phi(10090) as an example.
        */
        def phi:Unit = {
            implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
            for {
                regularTotient <- Future{
                    val initial = Instant.now
                    start.totient
                    Instant.now.toEpochMilli() - initial.toEpochMilli()
                }
                totientImprov <- Future {
                    val iniital = Instant.now
                    start.totientImprov
                    Instant.now.toEpochMilli - iniital.toEpochMilli
                }
            } yield println(s"regularTotient - ${regularTotient} ms totientImprov - ${totientImprov}")
            
        }

        /*
            P40
           (**) Goldbach's conjecture.
            Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given even integer.
            scala> 28.goldbach
            res0: (Int, Int) = (5,23)
        */
        def goldBach: (Int, Int) = {
            primes.takeWhile( _ < start ).find(p => (start-p).isPrime) match {
                case Some(p1) => (p1, start-p1)
                case None => throw new IllegalArgumentException
            }
            
            // my solutions
            // if(start % 2 == 0) {
            //     listPrimesinRange(3 to start-3).find(pr => (start - pr).isPrime ) match {
            //         case Some(prime) =>  (prime, start - prime)
            //         case None => throw new NoSuchElementException(s"There is no goldBach conjecture for ${start}")
            //     }
                
            // } else {
            //     throw  new IllegalArgumentException("Not able to find goldBach conjecture.")
            // }
        }


        

        

     
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


        /*
            P39
            A list of prime numbers.
            Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
            scala> listPrimesinRange(7 to 31)
            res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
        */
        def listPrimesinRange(range:Range): List[Int] = {
            primes.dropWhile{_ < range.start}.takeWhile{_ < range.end}.toList
            // range.toList.filter(_.isPrime)
        }

        /* 
            P41
            (**) A list of Goldbach compositions.
            Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
            scala> printGoldbachList(9 to 20)
            10 = 3 + 7
            12 = 5 + 7
            14 = 3 + 11
            16 = 3 + 13
            18 = 5 + 13
            20 = 3 + 17
            In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.

            Example (minimum value of 50 for the primes):

            scala> printGoldbachListLimited(1 to 2000, 50)
            992 = 73 + 919
            1382 = 61 + 1321
            1856 = 67 + 1789
            1928 = 61 + 1867
        */
        def printGoldBachListLimited(range:Range, limit:Int = 0): Unit = 
            getGoldBachRange(range).filter{ 
                case (n, (firstPrime, _)) => firstPrime >= limit
            }.foreach {
            case (n, (firstPrime,secondPrime)) =>  
                println(s"${n} = ${firstPrime} + ${secondPrime}")
        }

        def printGoldbachList(range:Range) : Unit = printGoldBachListLimited(range)
        
        // Drop while starts at the beginning of the stream and applies a predicate to each item, one by one. It doesn't start returning items in a new stream until it 
        // reaches an item that does not meet the predicate. Then it stops checking elements against the predicate and returns every item in teh stream from that point on.
        // TakeWhile starts at the beginning of the stream and applies a predicate to each item, one by one. It returns each item in a new stream until it reaches an item that doesn't meet the prediate.
        // TakeWhile doesn't return the item that fails to meet the predicate. It stops with the last item that meets the predicate.
        def getGoldBachRange(range:Range): Stream[(Int,(Int,Int))] = range.toStream.filter(r => {
            r > 2 && r % 2 == 0
        }).map(n => (n, n.goldBach))

        
    }
}





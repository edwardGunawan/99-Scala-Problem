object Problem27 extends App {
    /*
        Group the elements of a set into disjoint subsets.
        a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
        Example:

        scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
        b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

        Example:

        scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
        Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).

        You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
    */

    /*
        These will let you generate a subset of a lst. However, because helper(n-1,lst.tail) the unchosen part
        you also put n-1, instead of n, for also counting. It has the constraint for having n on how many value does
        each combination should have.

        In the generateSubSet m is k over here, the amount of choice that you have left
    */
    def generateSubSet[A](n: Int, lst:List[A]): List[List[A]] = {
        (n,lst) match {
            case (0,_) => List(Nil)
            case (_,Nil) => Nil
            case (m,head :: tail)=> generateSubSet(m-1,tail).map(head :: _) ::: generateSubSet(m,tail)
        }
    }

    // generating all subset, powerset
    def generateSubSet[A](lst:List[A]): List[List[A]] = {
        (lst) match {
            // case (0,_) => List(Nil)
            case Nil => List(Nil)
            case (head :: tail)=> generateSubSet(tail).map(head :: _) ::: generateSubSet(tail)
        }
    }

    /*
        Disjoint Set : Two set are named to be Disjoint set if they don't have anything in Common. Intersection will be an empty set
    */
    def group3[A](lst:List[A]): List[List[List[A]]] = for {
        two <- generateSubSet(2,lst)
        noTwo = lst.toSet diff(two.toSet) // get List that has noTwo elements in it
        three <- generateSubSet(3, noTwo.toList)
    } yield List(two, three, (noTwo.toSet diff three.toSet).toList /* The third one will also get the rest of the list*/)


    
    def group[A](lstGroup:List[Int], lst:List[A]) : List[List[List[A]]] = {
        lstGroup match {
            // if the lstGroup is the end of the list then return List(Nil) because Nil automatically will be regard to empty list
            // so the list :: lastList at the bottom will be doing List(1,2,3) :: List() => List(List(1,2,3))
            case Nil => List(Nil)
            // if there is no more list after the disjoint set, but there is stil lstGroup return an empty List(Nil)
            case _ if lst.isEmpty => List(Nil)
            // generate SubSet and then loop thorugh the all the generate field and call the recursive function again
            // using flatMap because the return statement after flatMap will be a List[List[List[List[A]]]] 
            case head :: tl => generateSubSet(head,lst).flatMap(list => {
                val noList = (lst.toSet diff list.toSet).toList
                group(tl, noList).map{lastList => 
                    list :: lastList // here it is List[List[A]] , because List(1,2,3) :: List(List(4)) => List(List(1,2,3), List(4))
                } // returning List[List[List[A]]] because calling group, then flatten so become List[List[A]]

                // here it will return List[List[List[A]]]
            }) // wrap again since generateSubSet return a List[List[List[List[A]].flatten]]
        }
    }

    def combination(len:Int, i:Int, lst:List[Int]):List[List[Int]] = {
        if(len == i) {
            List(Nil)
        }
        else {
            val includedList = List(lst.head) :: combination(len, i+1, lst.tail) // choose the current head
            val notIncludedList = combination(len, i+1, lst.tail) // skip the current head
            includedList ::: notIncludedList
        }
    }

    
    // println(group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
    val lst = List(1,2,3)
    println(generateSubSet(lst))
    

    // println(group3(lst))
    // println(group(List(1,2,3),lst))
}
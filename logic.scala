package logic {
    

    object S99Logic {
        implicit class S99LogicOps(a:Boolean) {
            /*
                
            P47 (*) Truth tables for logical expressions (2).
                Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit conversion from Boolean.) not will have to be left as a object method.
                scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
                A     B     result
                true  true  true
                true  false true
                false true  false
                false false false 
            */
            def and(b:Boolean): Boolean = S99Logic.and(a,b)
            def or(b:Boolean): Boolean = S99Logic.or(a,b)
            def xor(b: Boolean): Boolean = S99Logic.xor(a,b)
            def nand(b: Boolean): Boolean = S99Logic.nand(a,b)
            def nor(b:Boolean): Boolean = S99Logic.nor(a,b)
            def equ(b: Boolean): Boolean = S99Logic.equ(a,b)
            def impl(b:Boolean):Boolean = S99Logic.impl(a,b)
            

        }
        /*
        P46 (**) Truth tables for logical expressions.
            Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
            scala> and(true, true)
            res0: Boolean = true

            scala> xor(true. true)
            res1: Boolean = false
            A logical expression in two variables can then be written as an function of two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))

            Now, write a function called table2 which prints the truth table of a given logical expression in two variables.

            scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
            A     B     result
            true  true  true
            true  false true
            false true  false
            false false false
        */
        def and(a:Boolean , b :Boolean): Boolean = (a,b) match {
            case (true, true) => true
            case _ => false
        }
        def or(a:Boolean, b:Boolean): Boolean = (a,b) match {
            case (false ,false) => false
            case _ => true
        }
        def not(a:Boolean): Boolean = a match {
            case true => false
            case false => true
        }
        /*
            XOR: 
            Exclusive OR: 
            The true output results if one and only one of the input to the gate is true.
            Or Output true when input is different
            A B  A XOR B
            0 0     0
            0 1     1
            1 0     1
            1 1     0

            Algebra Expressions:  
                !((a || b) && (a && b))
                (a && !b) || (!a && b)
            
        */
        def xor(a:Boolean, b: Boolean): Boolean = not(equ(a,b))
        def nand(a:Boolean, b: Boolean): Boolean = not(and(a,b))
        def nor(a:Boolean, b:Boolean): Boolean = not(or(a,b))
        def equ(a:Boolean, b: Boolean): Boolean = or(and(a,b), and(not(a), not(b)))
        def impl(a:Boolean, b:Boolean):Boolean = or(not(a),b)

        def table2(func:(Boolean,Boolean) => Boolean): Unit = {
            println("A \t B \t result")
            for {
                a <- List(true,false)
                b <- List(true, false)
                res = func(a,b)
            } yield println(s"$a \t $b \t $res")
        }
    }
}
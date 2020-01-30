/*
    After having learned the different forms of patterns, you might be interested
    in seeing them applied in a larger example. The proposed task is to write an
    expression formatter class that displays an arithmetic expression in a twodimensional
    layout. Divisions such as “x / (x + 1)” should be printed vertically,
    by placing the numerator on top of the denominator, like this:
        x
    ---------
    x   +   1
*/
package com.edwardhuang.expr 
import com.edwardhuang.layout.Element

sealed abstract class Expr 
case class BinOp(operator:String, left:Expr, right:Expr) extends Expr
case class Var(name: String) extends Expr
case class Number(num:Double) extends Expr
case class UnOp(operator:String, arg:Expr) extends Expr

// The precedence of the oepration will be treated as a Set as the key and the precendence number as the value
class ExprFormatter {
    // Contains operators in groups of increasing precendence
    private val opGroups = 
        Array(
            Set("|", "||"),
            Set("&", "&&"),
            Set("^"),
            Set("==", "!="),
            Set("<", "<=", ">", ">="),
            Set("+", "-"),
            Set("*", "%")
        )
    
    // A mapping from operators to their precedence
    private val precedence = {
        val assocs = 
            for {
                i <- 0 until opGroups.length
                ops <- opGroups(i)
            } yield op -> i
        Map() ++ assocs // return a map value with key value pair
    }

    private val unaryPrecedence = opGroups.length
    // assign the division precedence to -1
    private val fractionPrecedence = -1

    private def format(e:Expr, enclPrec:Int): Element = e match {
        // if the expression is a variable, the result is an element formed from the variable's name
        case Var(name) => elem(name)
        // If the expression is a number, the result is an element formed from the number value
        // the stripdot function cleans up the display of a floating point number by stripping any ".0" suffix from string
        case Number(num) => 
            def stripDot(s:String) = 
                if (s endsWith ".0") s.substring(0,s.length - 2)
                else s
            elem(stripDot(num.toString))
        case UnOp(op, arg) => elem(op) beside format(arg, unaryPrecedence)
        // if the expression is a fraction,an intermediate result frac is formed by placing the formatted operands left and right on top of each other
        case BinOp("/", left, right) => 
            val top = format(left,fractionPrecedence)
            val bot = format(right fractionPrecedence)
            val line = elem('-', top.width max bot.width, 1) // to give more widen on the line
            val frac = top above line above bot
            if(enclPrec != fractionPrecedence) frac 
            else elem(" ") beside frac beside elem(" ")
        case BinOp(op,left, right) => 
            val opPrec = precedence(op)
            val l = format(left, opPrec)
            val r = format(right, opPrec+1)
            val oper = l beside elem(" " + op + " ") beside r 
            if (enclPrec <= opPrec) oper // you know if the enclPrec is more than the opPrec because you are nested inside another operators
            // as you go deep into the operation, you'll find that each precedence are more than the current once
            else elem("(") beside oper beside elem(")") // you need to put a parenthesis in between for nested oeprator
    }

    def format(e:Expr): Element = format(e, 0)


}




import munit.ScalaCheckSuite
import org.scalacheck._
import munit.Clue.generate

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary

import com.tkroman.kpi.y2022.l1._



class IntExpr extends ScalaCheckSuite {

    given [A: Arbitrary]: Arbitrary[Lit] =
    Arbitrary(arbitrary[Int].map(i => Lit(i)))

    
    property (" a*(b+c) == a*b+a*c "){
        forAll{(n1:Int, n2:Int, n3:Int)=>
            eval(Mul(Lit(n1),Add(Lit(n2),Lit(n3)))) == eval(Add(Mul(Lit(n1), Lit(n2)), Mul(Lit(n1), Lit(n3))))
        }
    }

    property("(Unary_-)^2 = Ɛ "){
        forAll{(n1:Int)=> 
            eval(UnMin(UnMin(Lit(n1)))) == n1
        }
    }

    property("Rightness of addition"){
        forAll{(n1:Int, n2:Int) => 
            eval(Add(Lit(n1), Lit(n2))) == n1+n2
        }
    }

    property("Rightness of multiplication"){
        forAll{(n1:Int, n2:Int) => 
            eval(Mul(Lit(n1), Lit(n2))) == n1*n2
        }
    }

    property("Rightness of Unary_-"){
            forAll{(n1:Int) =>  eval(UnMin(Lit(n1))) == (-n1)
        }
    }

    property("Just 1 more example"){
            forAll{(n1:Int, n2:Int, n3:Int, n4:Int, n5:Int) =>  
                eval(UnMin(Add(Lit(n1), Add(Lit(n2), Mul(Add(Lit(n3), Lit(n4)),Lit(n5)))))) == -(n1+(n2+((n3+n4)*n5)))
        }
    }

    property("n1*n1*n1 = n1^3"){
            forAll{(n1:Int) =>  
                eval(Mul(Mul(Lit(n1),Lit(n1)),Lit(n1))) == n1*n1*n1
            }
    }

}

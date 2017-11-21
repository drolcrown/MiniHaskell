import ast.{ExpSoma, ValorInteiro}
import org.scalatest._

class TesteExpSoma extends FlatSpec with Matchers {

  "An integer value 5 + an integer value 10" should "be evaluated to Valor(15)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val soma = new ExpSoma(val5, val10)

    soma.avaliar() should be (ValorInteiro(15))
  }

}

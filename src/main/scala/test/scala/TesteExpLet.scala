import ast._
import org.scalatest.{FlatSpec, Matchers}


class TesteExpLet extends FlatSpec with Matchers {

  info("Starting TestExpLet...")
  "A let x = 10 in x + x" should "be evaluated to Valor(20)" in {
    val let  = new ExpLet("x", ValorInteiro(10),
      new ExpSoma(new ExpRef("x"), new ExpRef("x")))


    let.avaliar() should be (ValorInteiro(20))
  }

}

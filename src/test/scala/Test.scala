import ast._
import org.scalatest._

class Test extends FlatSpec with Matchers {

  def testeExpSoma() = {
    info("Starting TestExpSoma...")
  "An integer value 5 + an integer value 10" should "be evaluated to Valor(15)" in {
    val val5 = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val soma = ExpSoma(val5, val10)

    soma.avaliar() should be(ValorInteiro(15))
    info("TestExpSoma evaluated...")
    }
  }

  def testeExpDiv() = {
    info("Starting TestExpDiv... 10 / 5 = 2")
    "An integer value 10 / an integer value 10" should "be evaluated to Valor(2)" in {
      val val5 = ValorInteiro(5)
      val val10 = ValorInteiro(10)

      val soma = ExpDiv(val10, val5)

      soma.avaliar() should be(ValorInteiro(2))
      info("TestExpDiv evaluated...")
    }

    info("Starting TestExpDiv... 10 / 0 = null")
    "An integer value 10 / an integer value 0" should "be evaluated to ValorNull()" in {
      val val0 = ValorInteiro(0)
      val val10 = ValorInteiro(10)

      val soma = ExpDiv(val10, val0)

      soma.avaliar() should be(ValorNull())
      info("TestExpDiv evaluated...")
    }
  }

  def testeExpSub() = {
    info("Starting TestExpSub...")
    "An integer value 10 - an integer value 5" should "be evaluated to Valor(5)" in {
      val val5 = ValorInteiro(5)
      val val10 = ValorInteiro(10)

      val soma = ExpSub(val10, val5)

      soma.avaliar() should be(ValorInteiro(5))
      info("TestExpSub evaluated...")
    }
  }

  def testeExpMult() = {
    info("Starting TestExpMult...")
    "An integer value 5 * an integer value 10" should "be evaluated to Valor(50)" in {
      val val5 = ValorInteiro(5)
      val val10 = ValorInteiro(10)

      val soma = ExpMult(val5, val10)

      soma.avaliar() should be(ValorInteiro(50))
      info("TestExpMult evaluated...")
    }
  }


  def testeExpLet() = {
    info("Starting TestExpLet...")
    "A let x = 10 in x + x" should "be evaluated to Valor(20)" in {
      val let = ExpLet("x", ValorInteiro(10),
         ExpSoma(ExpRef("x"), ExpRef("x")))

      let.avaliar() should be(ValorInteiro(20))
      info("TestExpLet evaluated...")
    }
  }

  def testeValores() = {
    info("Starting TestValores...")
    "An integer value 5" should "be evaluated to 5" in {
      val val5 = ValorInteiro(5)

      val5.avaliar() should be(ValorInteiro(5))
      info("TestValore evaluated...")
    }
  }

  //Roda todos testes
  testeExpSoma()
  testeExpSub()
  testeExpMult()
  testeExpDiv()
  testeExpLet()
  testeValores()
}

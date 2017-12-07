import ast._
import exceptions.TipoInvalido
import memoria.Ambiente
import org.scalatest.{FlatSpec, Matchers}

class TesteTipos extends FlatSpec with Matchers  {

  info("Iniciando Teste Tipos -> SOMA ...")
    "An boolean value true + an integer value 10" should "be evaluated to TErro()" in {
    val valB = ValorBooleano(true)
    val val10 = ValorInteiro(10)

    val soma = ExpSoma(valB, val10)

    soma.verificaTipo should be(TErro())
  }

  "An boolean value 10 + an integer value 10" should "be evaluated to Value(20)" in {
    val val10 = ValorInteiro(10)

    val soma = ExpSoma(val10, val10)

    soma.avaliar() should be(ValorInteiro(20))
    info("Fechando Teste Tipos -> SOMA...")
  }

  info("Iniciando Teste de Tipos -> LET ...")
  "A let x = 10 in x + value(5) " should "be evaluated to TInt()" in {
    val val5 = ValorInteiro(5)
    val val10 = ValorInteiro(10)
    val amb = Ambiente.atualiza("x", val10)

    val let1 = new ExpLet("x", val10 , ExpSoma (ExpRef("x"), val5))

    let1.verificaTipo should be(TInt())
    Ambiente.removeAmbiente()
  }

  "A let x = 10 in x + value boolean true " should "be evaluated to TErro()" in {
    val val10 = ValorInteiro(10)
    val amb = Ambiente.atualiza("x", val10)

    val let1 = new ExpLet("x", val10 , ExpSoma (ExpRef("x"), ValorBooleano(true)))

    let1.verificaTipo should be(TErro())
    Ambiente.removeAmbiente()
    info("Fechando Teste de Tipos -> LET...")
  }

  info("Iniciando Teste de Tipos -> LAMBDA ...")
  "A lambda expression  x ->  x + value(1) " should "be evaluated to TInt()" in {
    val val1 = ValorInteiro(1)
    val amb = Ambiente.atualiza("x", val1)

    val lambda = new ExpLambda("x", ExpSoma (ExpRef("x"), val1))

    lambda.verificaTipo should be(TArr(TInt(),TInt()))
    Ambiente.removeAmbiente()
  }

  "A lambda expression  x ->  x + true " should "be evaluated to TErro()" in {
    val val1 = ValorInteiro(1)
    val amb = Ambiente.atualiza("x", val1)

    val lambda = new ExpLambda("x", ExpSoma (ExpRef("x"), ValorBooleano(true)))

    lambda.verificaTipo should be(TArr(TInt(),TErro()))
    Ambiente.removeAmbiente()
    info("Fechando Teste de Tipos -> LAMBDA...")
  }

  info("Iniciando Teste de Tipos -> REF ...")
  "A expression  Ref(x) in x = 1" should "be evaluated to TInt()" in {
    val val1 = ValorInteiro(1)
    val amb = Ambiente.atualiza("x", val1)

    val ref = ExpRef("x")

    ref.verificaTipo should be(TInt())
    Ambiente.removeAmbiente()
  }

  "A expression  Ref(x) in x = true" should "be evaluated to TBool()" in {
    val val1 = ValorBooleano(true)
    val amb = Ambiente.atualiza("x", val1)

    val ref = ExpRef("x")

    ref.verificaTipo should be(TBool())
    Ambiente.removeAmbiente()
    info("Fechando Teste de Tipos -> REF...")
  }

  info("Iniciando Teste de Tipos -> APLICACAO NOMEADA ...")
  it should "throw TipoInvalido when Applying x = true in named application inc (x) = x + 1" in {
    val inc = DecFuncao("inc", List("x"), ExpSoma(ExpRef("x"), ValorInteiro(1)))
    Ambiente.iniciar()
    Ambiente.declararFuncao(inc)

    val app = ExpAplicacaoNomeada("inc", List(ValorBooleano(true)))

    intercept[TipoInvalido] {
      app.avaliar()
    }
  }

    "A Applying x = value(10) in named application inc (x) = x + 1" should "be evaluated to TInt()" in {
    val inc = DecFuncao("inc", List("x"), ExpSoma(ExpRef("x"), ValorInteiro(1)))
    Ambiente.iniciar()
    Ambiente.declararFuncao(inc)

    val app = ExpAplicacaoNomeada("inc", List(ValorInteiro(10)))

    app.verificaTipo should be (TInt())
    info("Fechando Teste de Tipos -> APLICACAO NOMEADA...")
  }

  info("Iniciando Teste de Tipos -> APLICACAO LAMBDA ...")
  "A Applying Application Lambda (x -> x + 2)  Value(10)" should "be evaluated to TInt()" in {
    Ambiente.iniciar()
    Ambiente.atualiza("x", ValorInteiro(2))
    val appL1 = new ExpAplicacaoLambda(new ExpLambda("x", ExpSoma(ExpRef("x"), ValorInteiro(2))), ValorInteiro(10))

    appL1.verificaTipo should be (TInt())
  }

  info("Iniciando Teste de Tipos -> APLICACAO LAMBDA ...")
  "A Applying Application Lambda (x -> x + 2) with true" should "be evaluated to TInt()" in {
    Ambiente.iniciar()
    Ambiente.atualiza("x", ValorInteiro(2))
    val appL1 = new ExpAplicacaoLambda(new ExpLambda("x", ExpSoma(ExpRef("x"), ValorInteiro(2))), ValorBooleano(true))

    appL1.verificaTipo should be (TErro())
    info("Fechando Teste de Tipos -> APLICACAO LAMBDA...")
  }

}

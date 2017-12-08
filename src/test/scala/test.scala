import ast._
import memoria.Ambiente

object test extends App{
  val inc = DecFuncao("inc", List("x", "y"), ExpSoma(ExpRef("x"), ExpRef("y")))
  Ambiente.iniciar()
  Ambiente.declararFuncao(inc)

  val app = ExpAplicacaoNomeada("inc", List(ValorInteiro(10), ValorInteiro(1)))
  println(app.avaliar())
}

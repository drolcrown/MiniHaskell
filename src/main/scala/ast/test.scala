package ast

import memoria.Ambiente

object test extends App{
  Ambiente.decFuncao("sqr", List("x", "y"),  ExpMult(ExpRef("x"), ExpRef("y")))
  Ambiente.decFuncao("sqr1", List("x"),  ExpMult(ExpRef("x"), ExpRef("x")))

  Ambiente.atualiza("x", ValorInteiro(1))
  Ambiente.atualiza("y", ValorInteiro(6))

  val valo = ExpMult(ValorInteiro(2), ValorInteiro(4))

  println(Ambiente.aplicaFuncao("sqr", List(ValorInteiro(5), ValorInteiro(5))))
}

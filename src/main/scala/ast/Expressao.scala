package ast

import memoria.Ambiente

trait Expressao {
  def avaliar() : Valor
}

case class ExpMult(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(v1.valor * v2.valor)
  }
}

case class ExpDiv(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(v1.valor / v2.valor)
  }
}

case class ExpSub(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(v1.valor - v2.valor)
  }
}

case class ExpSoma(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(v1.valor - v2.valor)
  }
}

case class ExpRef(arg : String) extends Expressao {
  override def avaliar() : Valor = {Ambiente.consulta(arg)}
}


case class ExpLet(val id : String, val expNomeada : Expressao, val corpo : Expressao) extends Expressao {
  override def avaliar(): Valor = {
    val valor = expNomeada.avaliar() // innermost strategy
    Ambiente.atualiza(id, valor)

    return corpo.avaliar()
  }
}
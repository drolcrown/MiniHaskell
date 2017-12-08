package ast

import exceptions.{ExpressaoInvalida, TipoInvalido, VariavelNaoDeclaradaException}
import memoria.Ambiente

trait Tipo

case class TErro() extends Tipo

case class TNull() extends Tipo

case class TInt() extends Tipo

case class TBool() extends Tipo

case class TArr(val t1: Tipo, val t2: Tipo) extends Tipo


trait Expressao {
  def avaliar() : Valor
  def verificaTipo : Tipo
}

case class ExpressoesPrimarias(){
  def verificaExpressoes(lhs: Expressao, rhs:Expressao): Tipo = {
    val t1 = lhs.verificaTipo
    val t2 = rhs.verificaTipo

    if(t1 == TInt() && t2 == TInt()) {
      return TInt()
    }
    return TErro()
  }
}

case class ExpMult(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(v1.valor * v2.valor)
  }

  override def verificaTipo: Tipo = {
    ExpressoesPrimarias().verificaExpressoes(lhs, rhs)
  }
}

case class ExpDiv(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]

    if (!rhs.avaliar().equals(ValorInteiro(0))) {
      val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]
      return ValorInteiro(v1.valor / v2.valor)
    } else {
      return ValorNull()
    }
  }

  override def verificaTipo: Tipo = {
    ExpressoesPrimarias().verificaExpressoes(lhs, rhs)
  }
}

case class ExpSub(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(v1.valor - v2.valor)
  }

  override def verificaTipo: Tipo = {
    ExpressoesPrimarias().verificaExpressoes(lhs, rhs)
  }
}

case class ExpSoma(val lhs: Expressao, val rhs:Expressao) extends Expressao {
  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(v1.valor + v2.valor)
  }

  override def verificaTipo: Tipo = {
    ExpressoesPrimarias().verificaExpressoes(lhs, rhs)
  }
}

case class ExpRef(arg : String) extends Expressao {
  override def avaliar(): Valor = {
    try {
      return Ambiente.consulta(arg)
    }
    catch {
      case ex: NoSuchElementException => throw VariavelNaoDeclaradaException()
    }
  }

  override def verificaTipo: Tipo = this.avaliar().verificaTipo
}

case class ExpLet(val id : String, val expNomeada : Expressao, val corpo : Expressao) extends Expressao {
  override def avaliar(): Valor = {
    val valor = expNomeada.avaliar() // innermost strategy
    Ambiente.atualiza(id, valor)

    return corpo.avaliar()
  }

  override def verificaTipo: Tipo = {
    if(expNomeada.verificaTipo == corpo.verificaTipo) {
      expNomeada.verificaTipo
    }
    else {
      TErro()
    }
  }
}

case class ExpAplicacaoNomeada(val nome: String, val argumentosAtuais : List[Expressao]) extends Expressao {
  override def avaliar(): Valor = {
    val decFuncao = Ambiente.recuperarFuncao(nome)
    var arg1 = argumentosAtuais; var arg2 = decFuncao.argFormal
    var tam = 0
    Ambiente.novoAmbiente()

    //Atualiza a lista de argumentos recebida
    while(arg1.nonEmpty && arg2.nonEmpty) {
      Ambiente.atualiza(arg2.head, arg1.head.avaliar())
      arg1 = arg1.tail
      arg2 = arg2.tail
    }

    var cont = 0
    for(arg <- argumentosAtuais){
      if(decFuncao.corpo.verificaTipo == arg.verificaTipo) {
        cont+= 1
      }
    }

    if(cont == argumentosAtuais.size) {
      val res = decFuncao.corpo.avaliar()
      Ambiente.removeAmbiente()
      return res
    }else {
      throw TipoInvalido()
    }
  }

  override def verificaTipo: Tipo = {this.avaliar().verificaTipo}

}

case class ExpLambda(val id : String, val corpo: Expressao) extends Expressao {

  override def avaliar(): Valor = {return Closure(id, corpo, Ambiente.ambienteAtual())}

  override def verificaTipo: Tipo = {return avaliar().verificaTipo}
}

case class DecFuncao(val nome: String, val argFormal : List[String], val corpo : Expressao)

case class ExpAplicacaoLambda(exp1 : Expressao, exp2 : Expressao) extends Expressao {

  override def avaliar(): Valor = {
    val v1 = exp1.avaliar()

    v1 match {
      case Closure(v, c, ambiente) => {
        Ambiente.novoAmbiente(ambiente)
        Ambiente.atualiza(v, exp2.avaliar())
        val res =  c.avaliar()
        Ambiente.removeAmbiente()
        return res
      }

      case _             => throw ExpressaoInvalida()
    }
  }

  override def verificaTipo: Tipo = {
    val t1 = exp1.verificaTipo
    t1 match {
      case TArr(m1, m2) if m2 == exp2.verificaTipo => m2
      case _ => TErro()
    }
  }
}


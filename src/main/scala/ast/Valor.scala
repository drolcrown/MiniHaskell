package ast

import memoria.Ambiente

import scala.collection.mutable

trait Valor extends Expressao

abstract class ValorConcreto[T](val valor : T) extends Valor {
  override def avaliar(): Valor = this
}

case class ValorBooleano(v : Boolean) extends ValorConcreto[Boolean](v){
  override def verificaTipo: Tipo = TBool()
}

case class ValorInteiro(v : Integer) extends ValorConcreto[Integer](v){
  override def verificaTipo: Tipo = TInt()
}

case class ValorNull() extends ValorConcreto[Null](null){
  override def verificaTipo: Tipo = TNull()
}

case class Closure(val id : String, val corpo : Expressao, ambiente : mutable.HashMap[String, Valor]) extends Valor {
  override def avaliar(): Valor = this

  override def verificaTipo: Tipo = {
    val t1 = Ambiente.consulta(id).verificaTipo
    val t2 = corpo.verificaTipo

    return TArr(t1, t2)
  }

}



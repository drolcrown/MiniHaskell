package memoria

import ast._

import scala.collection.mutable

object Ambiente {

  val tabela = new mutable.HashMap[String, Expressao]

  val ambienteProvisorio = new mutable.HashMap[String, Expressao]

  val tabelaFunc = new mutable.HashMap[String, (List[String], Expressao)]

  def atualiza(variavel : String, valor : Expressao): Unit = {
    tabela += (variavel -> valor)
  }

  def consulta(variavel : String) = {
    if(ambienteProvisorio.contains(variavel))
      ambienteProvisorio(variavel).avaliar()
    else
      tabela(variavel).avaliar()
  }

  def decFuncao(nome: String, arg : List[String], corpo : Expressao) = {
    tabelaFunc += (nome -> (arg, corpo))
  }

  def pesquisaFuncao(nome: String): (List[String], Expressao) = {tabelaFunc(nome)}

  def aplicaFuncao(nome: String, valor: List[Valor]): Valor = {
    var listaId = pesquisaFuncao(nome)._1
    var listaValor = valor
    //So atribui ids que possui valores
    while(listaValor.nonEmpty){
      if(listaId.nonEmpty){
        ambienteProvisorio += (listaId.head -> listaValor.head)
        listaValor = listaValor.tail
        listaId = listaId.tail
      }else{
        listaValor = listaValor.tail
      }
    }
    pesquisaFuncao(nome)._2.avaliar()
  }

}

package basic

import scala.collection.mutable

class SymbolTable {

  val hashMap: mutable.HashMap[String, Any] = mutable.HashMap()

  def setSymbol(symbol: String, value: Any): Unit = {
    this.hashMap(symbol) = value
  }

  def getValue(symbol: String): Any = {
    this.hashMap(symbol)
  }

}

//class BasicInterpreter(basicProgram: Seq[BasicStatement]) {
//
//  val symbolTable: SymbolTable = new SymbolTable()
//
//  def executePrint(items: Seq[BasicToken]): Unit = {
//    items.foreach {
//      case BasicToken.Identifier(id) => print(symbolTable.getValue(id))
//      case item => print(item.literal)
//    }
//  }
//
//  def execute(): Unit = {
//    basicProgram.foreach{ statement => {
//      statement.command match {
//        case BasicCommand.Print(items) => executePrint(items)
//      }
//    }}
//  }
//
//}

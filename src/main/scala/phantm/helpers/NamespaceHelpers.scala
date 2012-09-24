package phantm.helpers

import collection.mutable
import scala.Some

import phantm.ast.Trees._
import phantm.util.Positional

class NamespaceContext extends Cloneable {
  private var currentNS: NSDeclaration = NSRootDeclaration;
  private var importedNS = new mutable.HashMap[Identifier, List[Identifier]]

  def setNamespace(ns: NSDeclaration) {
    currentNS = ns.resolve(currentNS); Unit
  }

  def addUseStatement(use: UseStatement) {
    use.uses.foreach((it) => addUseDeclaration(it))
  }

  def addUseDeclaration(use: UseDeclaration) {
     val imported = Identifier("") :: use.ids
    use.alias match {
      case Some(alias: Identifier) => importedNS += alias -> imported
      case None => importedNS += imported.last -> imported
    }
  }

  def getImport(name: Identifier): List[Identifier] = {
    importedNS.get(name) match {
      case None =>
        currentNS.ids ::: List(name)
      case Some(imported)                 =>
           imported
    }
  }

  def getCurrent = currentNS

  override def clone   = super.clone().asInstanceOf[NamespaceContext]
}


trait QualifiedDecl extends Positional {
  val name: Identifier

  def qName(context: NamespaceContext): String = context.getCurrent.qNameOf(name)
}

trait QualifiedRef extends Positional {
  val name: Identifier
  val nsroot: NSRoot
  val nss: List[Identifier]

  def getIds(ctx: NamespaceContext): List[Identifier] = nsroot match {
    case NSCurrent => ctx.getCurrent.ids  ::: nss ::: List(name)
    case NSGlobal =>Identifier("") :: nss ::: List(name)
    case NSNone => ctx.getImport(name)
  }

  def namespace(ctx: NamespaceContext) = getIds(ctx).map {
    (id) => id.value
  }

  def qName(ctx: NamespaceContext) = {
    getIds(ctx).map {
      (n) => n.value
    }.mkString("\\")
  }
}

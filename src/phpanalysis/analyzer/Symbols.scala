package phpanalysis.analyzer

import scala.collection.mutable.HashMap
import parser.Trees._
import Types._


object Symbols {
  trait Symbolic {
    self =>

    var opt_sym: Option[Symbol] = None

    def setSymbol(sym: Symbol): self.type = { opt_sym = Some(sym); this }
    def hasSymbol: Boolean = opt_sym != None
    def getSymbol: Symbol = opt_sym  match {
        case Some(x) => x
        case None => scala.Predef.error("Cannot access undefined Symbol")
    }
  }

  def previousPos(x: Positional) = {
      if (x.file == None) {
          "internally"
      } else {
          "in "+x.getPos
      }
  }


  abstract class Symbol extends Positional {
    val id: Int = ID.next
    var overwriteable = false
    var userland = false
    val name: String

    def setOverwriteable: this.type = setOverwriteable(true)

    def setOverwriteable(value: Boolean): this.type = {
        overwriteable = value;
        this
    }

    def setUserland: this.type = setUserland(true)

    def setUserland(value: Boolean): this.type = {
        userland = value;
        this
    }
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }


  trait Scope {
    var variables: HashMap[String,VariableSymbol] = new HashMap[String,VariableSymbol]

    def getVariables: List[VariableSymbol] = variables map { x => x._2 } toList

    def lookupVariable(n: String): Option[VariableSymbol] = variables.get(n)

    def registerVariable(cs: VariableSymbol) : Unit = variables.get(cs.name) match {
      case None => variables += ((cs.name, cs))
      case Some(x) => /* no error */
    }

    def registerPredefVariables = {
        registerVariable(new VariableSymbol("GLOBALS"))
        registerVariable(new VariableSymbol("_GET"))
        registerVariable(new VariableSymbol("_FILES"))
        registerVariable(new VariableSymbol("_POST"))
        registerVariable(new VariableSymbol("_ENV"))
        registerVariable(new VariableSymbol("_COOKIE"))
        registerVariable(new VariableSymbol("_REQUEST"))
        registerVariable(new VariableSymbol("_SESSION"))
        registerVariable(new VariableSymbol("_SERVER"))
    }

  }

  object GlobalSymbols extends Scope {
    var classes: HashMap[String,ClassSymbol] = new HashMap[String,ClassSymbol]
    var ifaces: HashMap[String,IfaceSymbol] = new HashMap[String,IfaceSymbol]
    var functions: HashMap[String,FunctionSymbol] = new HashMap[String,FunctionSymbol]
    var constants: HashMap[String,ConstantSymbol] = new HashMap[String,ConstantSymbol]

    def lookupIface(n: String): Option[IfaceSymbol] = ifaces.get(n)

    def registerIface(is: IfaceSymbol) : Unit = ifaces.get(is.name) match {
      case None => ifaces += ((is.name, is))
      case Some(x) => Reporter.notice("Interface " + is.name + " already declared (previously declared "+previousPos(x)+")", is)
    }

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n.toLowerCase)

    def registerClass(cs: ClassSymbol) : Unit = classes.get(cs.name.toLowerCase) match {
      case None => classes += ((cs.name.toLowerCase, cs))
      case Some(x) => Reporter.notice("Class " + cs.name + " already declared (previously declared "+previousPos(x)+")", cs)
    }

    def lookupFunction(n: String): Option[FunctionSymbol] = functions.get(n.toLowerCase)

    def registerFunction(fs: FunctionSymbol) : Unit = {
        functions.get(fs.name.toLowerCase) match {
            case Some(x) =>
                if (!x.overwriteable) {
                    Reporter.notice("Function " + fs.name + " already declared (previously declared "+previousPos(x)+")", fs)
                }
                fs.importAPIFrom(x)
            case None =>
        }

        functions += ((fs.name.toLowerCase, fs))
    }

    def lookupConstant(n: String): Option[ConstantSymbol] = constants.get(n)

    def lookupOrRegisterConstant(id: Identifier): ConstantSymbol = {
        lookupConstant(id.value) match {
            case Some(cs) => cs
            case None =>
                // In case of an undefined constant, PHP falls back to its name as string value
                if (Main.verbosity > 0) {
                    Reporter.notice("Potentially undefined constant", id)
                }
                val cs = new ConstantSymbol(id.value, Some(PHPString(id.value)), TString)

                registerConstant(cs)

                cs
        }
    }

    def registerConstant(cs: ConstantSymbol) : Unit = constants.get(cs.name) match {
      case None => constants += ((cs.name, cs))
      case Some(x) => Reporter.notice("Function " + cs.name + " already declared (previously declared "+previousPos(x)+")", cs)
    }

    def getClasses: List[ClassSymbol] = classes map { x => x._2 } toList
    def getFunctions: List[FunctionSymbol] = functions map { x => x._2 } toList
    def getConstants: List[ConstantSymbol] = constants map { x => x._2 } toList
  }

  class FunctionSymbol(val name: String, var typ: Type) extends Symbol with Scope {
    val args = new HashMap[String, ArgumentSymbol]();
    var argList: List[(String, ArgumentSymbol)] = Nil;

    def importAPIFrom(fs: FunctionSymbol) {
        typ = fs.typ

        if (args.size == fs.args.size) {
            for (((_, arg1), (_, arg2)) <- argList.zip(fs.argList)) {
                arg1.typ = arg2.typ
                arg1.optional = arg2.optional || arg1.optional
            }

        } else {
            //TODO
        }
    }

    def registerArgument(as: ArgumentSymbol) = args.get(as.name) match {
        case Some(x) => Reporter.error("Argument "+as.name+" already defined (previously defined "+previousPos(x)+")", as)
        case None => args(as.name) = as; argList = argList ::: List((as.name, as));

    }

    override def toString = {
        name+argList.map {x => x._1}.mkString("(", ",", ")")
    }

    def getArgsVariables: List[VariableSymbol] = getArguments ::: super.getVariables

    override def lookupVariable(n: String): Option[VariableSymbol] = args.get(n) match {
        case Some(as) => Some(as)
        case None => variables.get(n)
    }

    override def registerVariable(vs: VariableSymbol) : Unit = args.get(vs.name) match {
        case Some(x) =>
        case None    =>
          variables.get(vs.name) match {
            case None => variables += ((vs.name, vs))
            case Some(x) => /* no error */
          }
    }

    def getArguments = argList map { x => x._2} toList
  }

  abstract class MemberVisibility {
    def stricterThan(o: MemberVisibility): Boolean;
  }
  object MVPublic extends MemberVisibility {
    override def toString = "public";
    override def stricterThan(o: MemberVisibility) = false
  }
  object MVPrivate extends MemberVisibility {
    override def toString = "private";
    override def stricterThan(o: MemberVisibility) = o != MVPrivate
  }
  object MVProtected extends MemberVisibility {
    override def toString = "protected";
    override def stricterThan(o: MemberVisibility) = o == MVPublic
  }

  class MethodSymbol(val cs: ClassSymbol, name: String, val visibility: MemberVisibility, typ: Type) extends FunctionSymbol(name, typ) {
    override def registerPredefVariables = {
        super.registerPredefVariables
        registerVariable(new VariableSymbol("this"))
    }

  }
  class PropertySymbol(val cs: ClassSymbol, name: String, val visibility: MemberVisibility, val typ: Type) extends VariableSymbol(name);
  class ClassConstantSymbol(val cs: ClassSymbol,  name: String, value: Option[Scalar], typ: Type) extends ConstantSymbol(name, value, typ);

  class IfaceMethodSymbol(val cs: IfaceSymbol, name: String, typ: Type, val visibility: MemberVisibility) extends FunctionSymbol(name, typ);
  class IfaceConstantSymbol(val cs: IfaceSymbol,  name: String, value: Option[Scalar], typ: Type) extends ConstantSymbol(name, value, typ);

  case class LookupResult[T](ms: Option[T], visibError: Option[MemberVisibility], staticClash: Boolean) {
      def isError = ms == None || visibError != None
  }

  class IfaceSymbol(val name: String, val parents: List[IfaceSymbol]) extends Symbol {
    val methods = new HashMap[String, IfaceMethodSymbol]();
    val constants = new HashMap[String, IfaceConstantSymbol]();
  }

  class ClassSymbol(val name: String, val parent: Option[ClassSymbol], ifaces: List[IfaceSymbol]) extends Symbol {
    val methods = new HashMap[String, MethodSymbol]();
    val properties = new HashMap[String, PropertySymbol]();
    val static_properties = new HashMap[String, PropertySymbol]();
    val constants = new HashMap[String, ClassConstantSymbol]();

    /* if a parent is defined and the method is defined in its parent, then the method can't be more restrictive */
    def registerMethod(ms: MethodSymbol) : Unit = methods.get(ms.name) match {
        case Some(x) => Reporter.error("Method "+name+"::"+ms.name+" already defined (previously defined "+previousPos(x)+")", ms)
        case None    =>
            parent match {
                case Some(pcs) => pcs.lookupMethod(ms.name, None) match {
                        case LookupResult(Some(pms), _, _) => {
                            if (ms.visibility stricterThan pms.visibility) {
                                Reporter.error("Method "+name+"::"+ms.name+" cannot overwrite "+pms.cs.name+"::"+pms.name+" with visibility "+ms.visibility+" (was "+pms.visibility+")", ms)
                            } else {
                                // todo: check prototypes
                                methods += ((ms.name, ms))
                            }
                        }
                        case LookupResult(None, _, _) => methods += ((ms.name, ms))
                    }
                case None => methods += ((ms.name, ms))
            }
    }
    /** looking up a method follows those rules:
     * 1) lookup the method localy, check PPP rules against the current scope
     * 2) if not found, lookup on the parent
     */

    def lookupMethod(name: String, from: Option[ClassSymbol]): LookupResult[MethodSymbol] = methods.get(name) match {
        case Some(ms) => ms.visibility match {
            case MVPublic => LookupResult(Some(ms), None, false)
            case MVProtected => from match {
                case Some(from_cs) =>
                    if (from_cs subclassOf this) {
                        LookupResult(Some(ms), None, false)
                    } else {
                        LookupResult(Some(ms), Some(MVProtected), false)
                    }
                case None =>
                    LookupResult(Some(ms), Some(MVProtected), false)
            }
            case MVPrivate => from match {
                case Some(from_cs) =>
                    if (from_cs == this) {
                        LookupResult(Some(ms), None, false)
                    } else {
                        LookupResult(Some(ms), Some(MVPrivate), false)
                    }
                case None =>
                    LookupResult(Some(ms), Some(MVPrivate), false)
            }
        }
        case None => parent match {
            case Some(pcs) => pcs.lookupMethod(name, from)
            case None => LookupResult(None, None, false)
        }

    }

    def registerConstant(cs: ClassConstantSymbol): Unit = constants.get(cs.name) match {
        case Some(x) => Reporter.notice("Class constant "+name+"::"+cs.name+" already declared (previously declared "+previousPos(x)+")", cs)
        case None    => constants += ((cs.name, cs))
    }

    def lookupConstant(name: String): Option[ClassConstantSymbol] = constants.get(name) match {
        case Some(x) => Some(x)
        case None => parent match {
            case Some(pcs) => pcs.lookupConstant(name)
            case None => None
        }
    }


    def registerStaticProperty(ps: PropertySymbol): Unit = properties.get(ps.name) match {
        case Some(x) => Reporter.notice("Property "+name+"::"+ps.name+" already declared (previously declared as a property "+previousPos(x)+")", ps)
        case None    => static_properties.get(ps.name) match {
            case Some(x) => Reporter.notice("Property "+name+"::"+ps.name+" already declared (previously declared as a static property "+previousPos(x)+")", ps)
            case None    => static_properties += ((ps.name, ps))
        }
    }

    def registerProperty(ps: PropertySymbol): Unit = static_properties.get(ps.name) match {
        case Some(x) => Reporter.notice("Property "+name+"::"+ps.name+" already declared (previously declared as a static property "+previousPos(x)+")", ps)
        case None    => properties.get(ps.name) match {
            case Some(x) => Reporter.notice("Property "+name+"::"+ps.name+" already declared (previously declared as a property "+previousPos(x)+")", ps)
            case None    => properties += ((ps.name, ps))
        }
    }

    private def lookupAnyProperty(name: String, from: Option[ClassSymbol], in: HashMap[String, PropertySymbol]): LookupResult[PropertySymbol] = in.get(name) match {
        case Some(ps) => ps.visibility match {
            case MVPublic => LookupResult(Some(ps), None, false)
            case MVProtected => from match {
                case Some(from_cs) => 
                    if (from_cs subclassOf this) {
                        LookupResult(Some(ps), None, false)
                    } else {
                        LookupResult(Some(ps), Some(MVProtected), false)
                    }
                case None =>
                    LookupResult(Some(ps), Some(MVProtected), false)
            }
            case MVPrivate => from match {
                case Some(from_cs) => 
                    if (from_cs == this) {
                        LookupResult(Some(ps), None, false)
                    } else {
                        LookupResult(Some(ps), Some(MVPrivate), false)
                    }
                case None =>
                    LookupResult(Some(ps), Some(MVPrivate), false)
            }
        }
        case None => parent match {
            // todo: Maybe copy all parent non-private properties?
            case Some(pcs) => pcs.lookupAnyProperty(name, from, in)
            case None => LookupResult(None, None, false)
        }

    }

    def lookupProperty(name: String, from: Option[ClassSymbol]): LookupResult[PropertySymbol] = lookupAnyProperty(name, from, properties)

    def lookupStaticProperty(name: String, from: Option[ClassSymbol]): LookupResult[PropertySymbol] = {
        val staticClash = lookupAnyProperty(name, from, properties) match {
            case LookupResult(Some(ps), Some(MVPrivate), _) => true
            case _ => false
        }

        lookupAnyProperty(name, from, static_properties) match {
            case LookupResult(res, visError, statClash) => LookupResult(res, visError, statClash || statClash)
        }
    }

    def subclassOf(target: ClassSymbol): Boolean = {
        if (this == target) true
        else parent match {
            case None => false
            case Some(cs) => cs subclassOf target
        }
    }

    def getMethods: List[MethodSymbol] = methods map { x => x._2 } toList
    def getConstants: List[ClassConstantSymbol] = constants map { x => x._2 } toList
    def getProperties: List[PropertySymbol] = properties map { x => x._2 } toList
    def getStaticProperties: List[PropertySymbol] = static_properties map { x => x._2 } toList

  }

  class ConstantSymbol(val name: String, val value: Option[Scalar], val typ: Type) extends Symbol
  class VariableSymbol(val name: String) extends Symbol
  class ArgumentSymbol(override val name: String, val byref: Boolean, var optional: Boolean, var typ: Type) extends VariableSymbol(name)

  def emitSummary = {
        def emitScope(s: Scope, p:String) = {
            for (val v <- s.getVariables) {
                println(p+"$"+v.name+"@"+v.id+"")
            }
        }
        for (val cs <- GlobalSymbols.getClasses) {
            print("Class "+cs.name+"@"+cs.id)
            cs.parent match {
                case Some(pcs) => println(" extends "+pcs.name+"@"+pcs.id+" {");
                case None => println(" {");
            }
            for (val cs <- cs.getConstants) {
                println("  const "+cs.name+"@"+cs.id);
            }
            for (val ps <- cs.getStaticProperties) {
                println("  static "+ps.visibility+" $"+ps.name+"@"+ps.id);
            }

            for (val ps <- cs.getProperties) {
                println("  "+ps.visibility+" $"+ps.name+"@"+ps.id);
            }

            for (val ms <- cs.getMethods) {
                println("  "+ms.visibility+" function "+ms.name+"@"+ms.id+" "+(ms.getArguments.map { x => "$"+x.name+"@"+x.id } mkString("(", ", ", ")"))+" {");
                emitScope(ms, "    ");
                println("  }");
            }
            println("}")
        }

        for (val fs <- GlobalSymbols.getFunctions) {
            println("function "+fs.name+"@"+fs.id+" "+(fs.getArguments.map { x => "$"+x.name+"@"+x.id } mkString("(", ", ", ")"))+"{");
            emitScope(fs, "  ");
            println("  }");
        }

        emitScope(GlobalSymbols, "")
  }
}

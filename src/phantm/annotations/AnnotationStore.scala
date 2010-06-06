package phantm.annotations

import phantm.symbols._
import phantm.types._

object AnnotationsStore {
    type FAnnots = (List[TFunction], List[Type])
    var functions = Map[String, FAnnots]().withDefaultValue((Nil, Nil));

    def clearFunctionAnnotations(fs: FunctionSymbol): FAnnots = {
        val ret = functions(fs.name)
        functions = functions + (fs.name -> (Nil, Nil))
        ret
    }

    def restoreFunctionAnnotations(fs: FunctionSymbol, annots: FAnnots) = {
        functions = functions + (fs.name -> annots)
    }

    def getReturnType(fs: FunctionSymbol): Type = {
        val data = functions(fs.name)

        if (data._2.size > 0) {
            data._2.reduceLeft(TypeLattice.join(_, _))
        } else {
            TNull
        }
    }

    def collectFunctionRet(fs: FunctionSymbol, t: Type) = {
        val data = functions(fs.name)

        functions += (fs.name -> (data._1, t :: data._2))
    }

    def collectFunction(fs: FunctionSymbol, ft: TFunction) = {
        val data = functions(fs.name)

        functions += (fs.name -> (ft :: data._1, data._2))
    }
}

package phantm.analyzer

import phantm.analyzer.Types.Type

abstract class TypeWideningFunction {
    def apply(t: Type): Type
}

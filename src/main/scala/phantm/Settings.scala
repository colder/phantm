package phantm

case class Settings(
    val verbosity: Int                = 1,
    val format: String                = "termbg",
    val resolveIncludes: Boolean      = true,
    val importAPI: Boolean            = true,
    val testsActive: Boolean          = false,
    val summaryOnly: Boolean          = false,
    val displayFixPoint: Boolean      = false,
    val displayIncludes: Boolean      = false,
    val displayProgress: Boolean      = false,
    val onlyLint: Boolean             = false,
    val typeFlowFilter: List[String]  = Nil,
    val includePaths: List[String]    = List("."),
    val apis: List[String]            = Nil,
    val dumps: List[String]           = Nil,
    val exportAPIPath: Option[String] = None,
    val exportCGPath: Option[String]  = None,
    val exportMGPath: Option[String]  = None,
    val inlineMode: InlineMode        = InlineManual,
    val compactErrors: Boolean        = true,
    val anyInput: Boolean             = false
)

sealed abstract class InlineMode;
case object InlineNone   extends InlineMode;
case object InlineLeaves extends InlineMode;
case object InlineFull   extends InlineMode;
case object InlineManual extends InlineMode;


object Settings {
    private var stgs: Option[Settings] = None

    def get = stgs.getOrElse(throw new RuntimeException("No global settings defined"))

    def set(stgs: Settings) = this.stgs = Some(stgs)
}

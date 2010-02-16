package phpanalysis;
import phpanalysis.parser.ParseNode;

trait Commented {
    self =>

    var comment: Option[String] = None

    def attachComment(com: Commented): self.type = {
        attachComment(com.comment);
    }

    def attachComment(com: Option[String]): self.type = {
        comment = com;

        this
    }
}

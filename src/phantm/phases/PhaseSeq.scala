package phantm.phases

sealed case class PhaseSeq(list: Seq[Phase] = Nil) {

    def andThen(p: Phase) =
        copy(list = list :+ p);

}

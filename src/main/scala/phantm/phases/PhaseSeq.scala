package phantm.phases

sealed case class PhaseSeq(list: Seq[Phase] = Nil) {

    def followedBy(p: Phase) =
        copy(list = list :+ p);

}

package common.automata

object SeqStack {
  implicit class SeqStack[A](seq: Seq[A]) {
    def push(v: A): Seq[A] = v +: seq
    def push(v: Seq[A]): Seq[A] = v ++ seq
    def pop(): Seq[A] = seq.drop(1)
  }
}

object SeqSplit {
  implicit class SeqSplitBy[A](seq: Seq[A]) {
    def splitBy(a: A): Seq[Seq[A]] = {
      seq.indexOf(a) match {
        case -1 => Seq(seq)
        case i =>
          val (left, right) = seq.splitAt(i)
          left +: right.drop(1).splitBy(a)
      }
    }
  }
}
package automata

object SeqStack {
  implicit class SeqStack[A](seq: Seq[A]) {
    def push(v: A): Seq[A] = v +: seq
    def push(v: Seq[A]): Seq[A] = v ++ seq
    def pop(): Seq[A] = seq.drop(1)
  }
}

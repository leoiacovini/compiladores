package automata


object NDFARunner {
  def fromNDFA[Input, State](ndfa: NonDeterministicFiniteAutomata[Input, State]): NDFARunner[Input, State] = {
    NDFARunner(ndfa, ndfa.initialStates)
  }
}
case class NDFARunner[Input, State](ndfa: NonDeterministicFiniteAutomata[Input, State], currentStates: Seq[State]) {
  def run(input: Input): Seq[State] = {
    for {
      state <- currentStates
      newStates <- ndfa.transition(state, input)
    } yield newStates
  }

  def withStates(newStates: Seq[State]): NDFARunner[Input, State] = copy(currentStates = newStates)
  def isAccepted: Boolean = {
    currentStates.exists(ndfa.acceptStates.contains)
  }
}
object NDFAStack {
  def accepts[Input, State, MachineIdentifier](ndfaStack: NDFAStack[Input, State, MachineIdentifier], symbols: Seq[Input]): Boolean = {
    symbols.foldLeft(ndfaStack) {case (stack, symbol) =>
      stack.run(symbol)
    }.isAccepted
  }
}
case class NDFAStack[Input, State, MachineIdentifier](ndfaMap: Map[MachineIdentifier, NonDeterministicFiniteAutomata[Input, State]],
                                                      submachineCalls: PartialFunction[(MachineIdentifier, State, Input), (MachineIdentifier, State)],
                                                      stack: Seq[(MachineIdentifier, NDFARunner[Input, State])]) {

  def push(id: MachineIdentifier, ndfaRunner: NDFARunner[Input, State]): NDFAStack[Input, State, MachineIdentifier] = copy(stack = (id -> ndfaRunner) +: stack)
  def pop(): NDFAStack[Input, State, MachineIdentifier] = copy(stack = stack.drop(1))

  def start(machineId: MachineIdentifier): NDFAStack[Input, State, MachineIdentifier] = push(machineId, NDFARunner.fromNDFA(ndfaMap(machineId)))

  private def existsSubmachineFor(machineId:MachineIdentifier, states: Seq[State], input: Input): Boolean = {
    states.exists(s => submachineCalls.isDefinedAt(machineId, s, input))
  }

  private def getSubmachine(machineId:MachineIdentifier, states:Seq[State], input: Input): (MachineIdentifier, State) = {
    states.flatMap(s => submachineCalls.lift(machineId, s, input)).head
  }

  private def fail() = copy(stack = Seq.empty)

  private def runStack(machineId: MachineIdentifier, ndfaRunner: NDFARunner[Input, State], input: Input): NDFAStack[Input, State, MachineIdentifier] = {
    println {
      s"""
         |runStack!
         |this: $this
         |ndfaRunner: $ndfaRunner
         |stack: \n${stack.map(x => x._1 + " -> " + x._2).mkString("\n")}
         |input: $input
       """.stripMargin
    }

    ndfaRunner.run(input) match {
      case empty if empty.isEmpty && existsSubmachineFor(machineId, ndfaRunner.currentStates, input) =>

        val (submachine, newState) = getSubmachine(machineId, ndfaRunner.currentStates, input)
        println(s"Entering submachine $submachine")
        val newRunner: NDFARunner[Input, State] = NDFARunner.fromNDFA(ndfaMap(submachine))
//        val newStates = newRunner.run(input)
        this.pop()
          .push(machineId, ndfaRunner.withStates(Seq(newState)))
          .push(submachine, newRunner)
      case empty if empty.isEmpty && stack.headOption.exists(_._2.isAccepted) =>
        stack.length match {
          case i if i <= 1 =>
            println(s"Nowhere to go, input $input")
            fail()
          case i if i > 1 =>
            val popped = this.pop()
            val (oldMachine, oldRunner) = popped.stack.head
            println(
              x =
                s"""
                    |oldRunner: $oldRunner
                    |ndfaRunner: $ndfaRunner
                    |ndfaStack: $stack
                    |topStack: ${stack.headOption}
                    |input: $input
              """.stripMargin)
//            val (_, newState) = oldRunner.currentStates.map(s => submachineCalls.lift(s, input)).head.get
            println("Leaving machine")
            popped
//            popped.runStack(oldMachine, oldRunner, input)
//            popped// .pop() //.push(oldRunner.withStates(Seq(newState)))
        }
      case empty if empty.isEmpty =>
        println {
          s"""
             |ndfaRunner: $ndfaRunner
             |ndfaStack: $stack
             |topStack: ${stack.headOption}
             |acceptedStates: ${stack.headOption.map(_._2.ndfa.acceptStates)}
             |currentStates: ${stack.headOption.map(_._2.currentStates)}
             |input: $input
            """.stripMargin
        }
        fail()
//        throw new Exception("No transition/submachineCall found")

      case notEmpty if notEmpty.nonEmpty =>
        val newRunner = ndfaRunner.withStates(notEmpty)
        this.pop().push(machineId, newRunner)
    }
  }
  def isAccepted: Boolean = {
    stack.length == 1 && stack.headOption.exists {case (mId, runner) => runner.isAccepted }
  }
  def run(input: Input): NDFAStack[Input, State, MachineIdentifier] = {
    stack.headOption match {
      case Some((machineId, ndfaRunner)) => runStack(machineId, ndfaRunner, input)

      case None => fail()
    }
  }
}

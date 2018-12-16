package basic

import org.scalatest.WordSpec

class BasicCommandTest extends WordSpec {

  "Print Command" in {
    val printCommand = Seq(
      BasicToken.Print(),
      BasicToken.Number("10"),
      BasicToken.Delimiter(","),
      BasicToken.Number("20")
    )

    assert(BasicCommand.fromTokensLine(printCommand) ==
      BasicCommand.Print(Seq(BasicToken.Number("10"), BasicToken.Number("20"))))
  }

  "Assign Command" in {
    val assignCommand = Seq(
      BasicToken.Let(),
      BasicToken.Identifier("J"),
      BasicToken.Equal(),
      BasicToken.Number("20")
    )

    assert(BasicCommand.fromTokensLine(assignCommand) ==
      BasicCommand.Assign(BasicToken.Identifier("J"), Seq(BasicToken.Number("20"))))

    val assignExpression = Seq(
      BasicToken.Let(),
      BasicToken.Identifier("J"),
      BasicToken.Equal(),
      BasicToken.Number("20"),
      BasicToken.Plus(),
      BasicToken.Identifier("A")
    )

    assert(BasicCommand.fromTokensLine(assignExpression) ==
      BasicCommand.Assign(BasicToken.Identifier("J"), Seq(BasicToken.Number("20"), BasicToken.Plus(), BasicToken.Identifier("A"))))
  }

  "Goto Command" in {
    val gotoCommand = Seq(
      BasicToken.Goto(),
      BasicToken.Number("10")
    )

    assert(BasicCommand.fromTokensLine(gotoCommand) ==
      BasicCommand.Goto(BasicToken.Number("10")))
  }

  "Read Command" in {
    val readCommand = Seq(
      BasicToken.Read(),
      BasicToken.Identifier("A1"),
      BasicToken.Delimiter(","),
      BasicToken.Identifier("A2"),
      BasicToken.Delimiter(","),
      BasicToken.Identifier("A3")
    )

    assert(BasicCommand.fromTokensLine(readCommand) ==
      BasicCommand.Read(Seq(BasicToken.Identifier("A1"), BasicToken.Identifier("A2"), BasicToken.Identifier("A3"))))
  }

  "Data Command" in {
    val dataCommand = Seq(
      BasicToken.Data(),
      BasicToken.Number("10"),
      BasicToken.Delimiter(","),
      BasicToken.Number("20"),
      BasicToken.Delimiter(","),
      BasicToken.Number("30")
    )

    assert(BasicCommand.fromTokensLine(dataCommand) ==
      BasicCommand.Data(Seq(BasicToken.Number("10"), BasicToken.Number("20"), BasicToken.Number("30"))))
  }

  "Gosub Command" in {
    val gosubCommand = Seq(
      BasicToken.GoSub(),
      BasicToken.Number("10")
    )

    assert(BasicCommand.fromTokensLine(gosubCommand) ==
      BasicCommand.GoSub(BasicToken.Number("10")))
  }

  "Return Command" in {
    val returnCommand = Seq(BasicToken.Return())

    assert(BasicCommand.fromTokensLine(returnCommand) == BasicCommand.Return())
  }

  "Next Command" in {
    val nextCommand = Seq(
      BasicToken.Next(),
      BasicToken.Identifier("A")
    )

    assert(BasicCommand.fromTokensLine(nextCommand) == BasicCommand.Next(BasicToken.Identifier("A")))
  }

  "If Command" in {
    val ifCommand = Seq(
      BasicToken.If(),
      BasicToken.Identifier("A"),
      BasicToken.Greater(),
      BasicToken.Number("10"),
      BasicToken.Then(),
      BasicToken.Number("20")
    )

    assert(BasicCommand.fromTokensLine(ifCommand) ==
      BasicCommand.If(
        Seq(BasicToken.Identifier("A")),
        Seq(BasicToken.Number("10")),
        BasicToken.Greater(),
        BasicToken.Number("20")))
  }

  "For Command" in {
    val forCommand = Seq(
      BasicToken.For(),
      BasicToken.Identifier("A"),
      BasicToken.Equal(),
      BasicToken.Identifier("X"),
      BasicToken.Plus(),
      BasicToken.Number("1"),
      BasicToken.To(),
      BasicToken.Number("20")
    )

    assert(BasicCommand.fromTokensLine(forCommand) ==
      BasicCommand.For(
        BasicToken.Identifier("A"),
        Seq(BasicToken.Identifier("X"), BasicToken.Plus(), BasicToken.Number("1")),
        Seq(BasicToken.Number("20"))))
  }

  "Remark Command" in {
    val remarkCommand = Seq(
      BasicToken.Rem(),
      BasicToken.Text("aaaaaa")
    )

    assert(BasicCommand.fromTokensLine(remarkCommand) == BasicCommand.Remark(BasicToken.Text("")))
  }

}

@main def entrypoint() =
  // inputs:
  val p1TestInput = FileLoader.readFileLines("input-test-p1.txt")
  val p2TestInput = FileLoader.readFileLines("input-test-p2.txt")
  val mainInput   = FileLoader.readFileLines("input.txt")

  // test results:
  val p1TestResult = d4p1.solve(p1TestInput)
  val p2TestResult = d4p2.solve(p2TestInput)

  // main results:
  val p1Result = d4p1.solve(mainInput)
  val p2Result = d4p2.solve(mainInput)

  println("Day 4:")
  println("-" * 20)
  println(s"[TEST] P1: $p1TestResult")
  println(s"[ACTUAL] P1: $p1Result")
  println("-" * 20)
  println(s"[TEST] P2: $p2TestResult")
  println(s"[ACTUAL] P2: $p2Result")
  println("-" * 20)
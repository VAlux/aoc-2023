@main def entrypoint() =
  // inputs:
  val p1TestInput = FileLoader.readFile("input-test-p1.txt")
  val p2TestInput = FileLoader.readFile("input-test-p2.txt")
  val mainInput   = FileLoader.readFile("input.txt")

  // test results:
  val p1TestResult = d1p1.solve(p1TestInput)
  val p2TestResult = d1p2.solve(p2TestInput)

  // main results:
  val p1Result = d1p1.solve(mainInput)
  val p2Result = d1p2.solve(mainInput)

  println("-" * 20)
  println(s"[TEST] P1: $p1TestResult")
  println(s"[ACTUAL] P1: $p1Result")
  println("-" * 20)
  println(s"[TEST] P2: $p2TestResult")
  println(s"[ACTUAL] P2: $p2Result")
  println("-" * 20)

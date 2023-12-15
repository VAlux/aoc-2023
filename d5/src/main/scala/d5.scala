@main def entrypoint() =
  // inputs:
  val p1TestInput = FileLoader.readFileLines("input-test-p1.txt")
  val p2TestInput = FileLoader.readFileLines("input-test-p2.txt")
  val mainInput   = FileLoader.readFileLines("input.txt")

  // test results:
  val p1TestResult = d5p1.solve(p1TestInput)
  val p2TestResult = d5p2.solve(p2TestInput)

  // main results:
  val runMain  = true
  val p1Result = if runMain then d5p1.solve(mainInput) else 0
  val p2Result = if runMain then d5p2.solve(mainInput) else 0

  println("Day 5:")
  println("-" * 20)
  println(s"[TEST] P1: $p1TestResult")
  println(s"[ACTUAL] P1: $p1Result")
  println("-" * 20)
  println(s"[TEST] P2: $p2TestResult")
  println(s"[ACTUAL] P2: $p2Result")
  println("-" * 20)

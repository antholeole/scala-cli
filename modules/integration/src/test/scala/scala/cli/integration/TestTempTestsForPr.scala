package scala.cli.integration

import java.io.File


class TestTempTestsForPr extends TestTestDefinitions with TestDefault {
  test("same file") {
    val inputs = TestInputs(
      os.rel / "TestInSource.scala" ->
        """//> using dep "org.scalameta::munit::0.7.27"
          |
          |object ExampleAPI:
          |   def foo(): Int = 4
          |
          |class ExampleSpec extends munit.FunSuite:
          |   test("check can use example API") {
          |       assertEquals(NumApi.foo(), 3)
          |   }
            """.stripMargin
    )

    inputs.fromRoot { root =>
      val compileRes = os.proc(TestUtil.cli, "test", "--print-class-path", baseExtraOptions, ".")
        .call(cwd = root)
      compileRes.out.trim().split(File.pathSeparator).foreach(println)

      os.proc(TestUtil.cli, "test", baseExtraOptions, ".")
        .call(cwd = root, stdout = os.Inherit)
    }
  }

  test("test-cmd") {
    val inputs = TestInputs(
      os.rel / "MyTest.test.scala" ->
        """//> using test.dep "org.scalameta::munit::0.7.27"
          |
          |class ExampleSpec extends munit.FunSuite:
          |   test("check can use example API") {
          |       assertEquals(3, 3)
          |   }
          |
          |class SomeOtherClass:
          |   def doStuff() =
          |     println("hi")
          |
            """.stripMargin
    )

    inputs.fromRoot { root =>
      os.proc(TestUtil.cli, "test", baseExtraOptions, ".")
        .call(cwd = root, stdout = os.Inherit)
    }
  }
}

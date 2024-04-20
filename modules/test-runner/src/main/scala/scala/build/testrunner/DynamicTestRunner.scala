package scala.build.testrunner

import sbt.testing._

import java.lang.annotation.Annotation
import java.lang.reflect.Modifier
import java.nio.file.{Files, Path}
import java.util.ServiceLoader
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

// TODO rename all occurances to something like "DyanmicTestDetector"
object DynamicTestRunner {

  case class NoTestFrameworkFoundException()
      extends Exception("No test framework found")

  def testRunnerClassPathAndLoader() = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val classPath   = TestRunner.classPath(classLoader)

    (classLoader, classPath)
  }

  def findFramework(
    testFrameworkOpt: Option[String],
    classLoader: ClassLoader,
    testRunnerClassPath: Seq[Path]
  ): Either[NoTestFrameworkFoundException, Framework] =
    testFrameworkOpt.map(loadFramework(classLoader, _))
      .orElse(findFrameworkService(classLoader))
      .orElse(findFramework(testRunnerClassPath, classLoader, TestRunner.commonTestFrameworks))
      .toRight(NoTestFrameworkFoundException())

  def findTestClasses(
    framework: Framework,
    classLoader: ClassLoader,
    testRunnerClassPath: Seq[Path]
  ): Iterator[(Class[_], Fingerprint)] = {
    val fingerprints = framework.fingerprints
    val classes = listClasses(testRunnerClassPath)
      .map(name => classLoader.loadClass(name))

    classes.flatMap { cls =>
      matchFingerprints(classLoader, cls, fingerprints)
        .map((cls, _))
        .iterator
    }
  }

  // adapted from https://github.com/com-lihaoyi/mill/blob/ab4d61a50da24fb7fac97c4453dd8a770d8ac62b/scalalib/src/Lib.scala#L156-L172
  private def matchFingerprints(
    loader: ClassLoader,
    cls: Class[_],
    fingerprints: Array[Fingerprint]
  ): Option[Fingerprint] = {
    val isModule               = cls.getName.endsWith("$")
    val publicConstructorCount = cls.getConstructors.count(c => Modifier.isPublic(c.getModifiers))
    val noPublicConstructors   = publicConstructorCount == 0
    val definitelyNoTests = Modifier.isAbstract(cls.getModifiers) ||
      cls.isInterface ||
      publicConstructorCount > 1 ||
      isModule != noPublicConstructors
    if (definitelyNoTests)
      None
    else
      fingerprints.find {
        case f: SubclassFingerprint =>
          f.isModule == isModule &&
          loader.loadClass(f.superclassName())
            .isAssignableFrom(cls)

        case f: AnnotatedFingerprint =>
          val annotationCls = loader.loadClass(f.annotationName())
            .asInstanceOf[Class[Annotation]]
          f.isModule == isModule && (
            cls.isAnnotationPresent(annotationCls) ||
            cls.getDeclaredMethods.exists(_.isAnnotationPresent(annotationCls)) ||
            cls.getMethods.exists { m =>
              m.isAnnotationPresent(annotationCls) &&
              Modifier.isPublic(m.getModifiers())
            }
          )
      }
  }

  def listClasses(classPathEntry: Path, keepJars: Boolean): Iterator[String] =
    if (Files.isDirectory(classPathEntry)) {
      var stream: java.util.stream.Stream[Path] = null
      try {
        stream = Files.walk(classPathEntry, Int.MaxValue)
        stream
          .iterator
          .asScala
          .filter(_.getFileName.toString.endsWith(".class"))
          .map(classPathEntry.relativize(_))
          .map { p =>
            val count = p.getNameCount
            (0 until count).map(p.getName).mkString(".")
          }
          .map(_.stripSuffix(".class"))
          .toVector // fully consume stream before closing it
          .iterator
      }
      finally if (stream != null) stream.close()
    }
    else if (keepJars && Files.isRegularFile(classPathEntry)) {
      import java.util.zip._
      var zf: ZipFile = null
      try {
        zf = new ZipFile(classPathEntry.toFile)
        zf.entries
          .asScala
          // FIXME Check if these are files too
          .filter(_.getName.endsWith(".class"))
          .map(ent => ent.getName.stripSuffix(".class").replace("/", "."))
          .toVector // full consume ZipFile before closing it
          .iterator
      }
      finally if (zf != null) zf.close()
    }
    else Iterator.empty

  // "keepJars" looks into dependencies; this is much slower and defaults to false.
  def listClasses(classPath: Seq[Path], keepJars: Boolean = false): Iterator[String] =
    classPath.iterator.flatMap(listClasses(_, keepJars))

  def findFrameworkService(loader: ClassLoader): Option[Framework] =
    ServiceLoader.load(classOf[Framework], loader)
      .iterator()
      .asScala
      .take(1)
      .toList
      .headOption

  def loadFramework(
    loader: ClassLoader,
    className: String
  ): Framework = {
    val cls         = loader.loadClass(className)
    val constructor = cls.getConstructor()
    constructor.newInstance().asInstanceOf[Framework]
  }

  def findFramework(
    classPath: Seq[Path],
    loader: ClassLoader,
    preferredClasses: Seq[String]
  ): Option[Framework] = {
    val frameworkCls = classOf[Framework]
    (preferredClasses.iterator ++ listClasses(classPath, true))
      .flatMap { name =>
        val it: Iterator[Class[_]] =
          try Iterator(loader.loadClass(name))
          catch {
            case _: ClassNotFoundException | _: UnsupportedClassVersionError | _: NoClassDefFoundError | _: IncompatibleClassChangeError =>
              Iterator.empty
          }
        it
      }
      .flatMap { cls =>
        def isAbstract = Modifier.isAbstract(cls.getModifiers)
        def publicConstructorCount =
          cls.getConstructors.count { c =>
            Modifier.isPublic(c.getModifiers) && c.getParameterCount() == 0
          }
        val it: Iterator[Class[_]] =
          if (frameworkCls.isAssignableFrom(cls) && !isAbstract && publicConstructorCount == 1)
            Iterator(cls)
          else
            Iterator.empty
        it
      }
      .flatMap { cls =>
        try {
          val constructor = cls.getConstructor()
          Iterator(constructor.newInstance().asInstanceOf[Framework])
        }
        catch {
          case _: NoSuchMethodException => Iterator.empty
        }
      }
      .take(1)
      .toList
      .headOption
  }

  /** Based on junit-interface [GlobFilter.
    * compileGlobPattern](https://github.com/sbt/junit-interface/blob/f8c6372ed01ce86f15393b890323d96afbe6d594/src/main/java/com/novocode/junit/GlobFilter.java#L37)
    *
    * @return
    *   Pattern allows to regex input which contains only *, for example `*foo*` match to
    *   `MyTests.foo`
    */
  private def globPattern(expr: String): Pattern = {
    val a = expr.split("\\*", -1)
    val b = new StringBuilder()
    for (i <- 0 until a.length) {
      if (i != 0) b.append(".*")
      if (a(i).nonEmpty) b.append(Pattern.quote(a(i).replaceAll("\n", "\\n")))
    }
    Pattern.compile(b.toString)
  }

  def main(args: Array[String]): Unit = {

    val (testFrameworkOpt, requireTests, verbosity, testOnly, args0) = {
      @tailrec
      def parse(
        testFrameworkOpt: Option[String],
        reverseTestArgs: List[String],
        requireTests: Boolean,
        verbosity: Int,
        testOnly: Option[String],
        args: List[String]
      ): (Option[String], Boolean, Int, Option[String], List[String]) =
        args match {
          case Nil => (testFrameworkOpt, requireTests, verbosity, testOnly, reverseTestArgs.reverse)
          case "--" :: t =>
            (testFrameworkOpt, requireTests, verbosity, testOnly, reverseTestArgs.reverse ::: t)
          case h :: t if h.startsWith("--test-framework=") =>
            parse(
              Some(h.stripPrefix("--test-framework=")),
              reverseTestArgs,
              requireTests,
              verbosity,
              testOnly,
              t
            )
          case h :: t if h.startsWith("--test-only=") =>
            parse(
              testFrameworkOpt,
              reverseTestArgs,
              requireTests,
              verbosity,
              Some(h.stripPrefix("--test-only=")),
              t
            )
          case h :: t if h.startsWith("--verbosity=") =>
            parse(
              testFrameworkOpt,
              reverseTestArgs,
              requireTests,
              h.stripPrefix("--verbosity=").toInt,
              testOnly,
              t
            )
          case "--require-tests" :: t =>
            parse(testFrameworkOpt, reverseTestArgs, true, verbosity, testOnly, t)
          case h :: t =>
            parse(testFrameworkOpt, h :: reverseTestArgs, requireTests, verbosity, testOnly, t)
        }

      parse(None, Nil, false, 0, None, args.toList)
    }

    val (classLoader, testRunnerClassPath) = testRunnerClassPathAndLoader()
    val runFailed =
      for {
        framework <- findFramework(testFrameworkOpt, classLoader, testRunnerClassPath)
        clsFingerprints = findTestClasses(framework, classLoader, testRunnerClassPath)

        runner = framework.runner(args0.toArray, Array(), classLoader)

        taskDefs = clsFingerprints
          .filter {
            case (cls, _) =>
              testOnly.forall(pattern =>
                globPattern(pattern).matcher(cls.getName.stripSuffix("$")).matches()
              )
          }
          .map {
            case (cls, fp) =>
              new TaskDef(cls.getName.stripSuffix("$"), fp, false, Array(new SuiteSelector))
          }
          .toVector
        initialTasks = runner.tasks(taskDefs.toArray)
        events       = TestRunner.runTasks(initialTasks.toIndexedSeq, System.out)
        _            = runner.done().map(doneMsg => System.out.println(doneMsg))

        _ <- if (requireTests && events.isEmpty)
          Left(new Exception("Error: no tests were run."))
        else Right(())
      } yield events.exists { ev =>
        ev.status == Status.Error ||
        ev.status == Status.Failure ||
        ev.status == Status.Canceled
      }

    runFailed match {
      case Left(e) => if (verbosity >= 2)
          sys.error(e.getMessage)
        else {
          System.err.println(e.getMessage)
          sys.exit(1)
        }
      case Right(failed) if failed =>
        sys.exit(1)
      case _ => ()
    }
  }
}

abstract class DynamicTestRunner

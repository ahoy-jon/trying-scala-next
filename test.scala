//> using dep com.lihaoyi::os-lib:0.9.3
//> using scala 3.5.0-RC1
//> using dep com.softwaremill.ox::core:0.0.25

import ox.*
import ox.syntax.*
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.capability

import scala.language.experimental.erasedDefinitions
import scala.language.experimental.captureChecking


@capability class State:
  private val run = new AtomicInteger(0)
  def newRun(): Int =
    run.incrementAndGet()


@capability trait Memoizer:
  //memoize will make the function localy deterministic, for any x, val f2 = memoize(f), f2(x) == f2(x)
  //but memoize stays in the captured context
  def memoize[A,B](f: A => B): (A -> B)^{this}


extension [A,B](f: A=>B)
  def memoize(using memo:Memoizer): (A -> B)^{memo} = summon[Memoizer].memoize(f)

def something()(using Ox, State):(Int,Int) =
  val run = summon[State].newRun()
  var n = 0
  def working() =
    while true do
      Thread.sleep(1)
      n = n + 1

  working().fork
  Thread.sleep(40)
  (run, n)



@capability class Random:
  inline def nextInt(n:Int):Int = scala.util.Random().nextInt(n)

object Random:
  inline def nextInt(n:Int)(using Random):Int = summon[Random].nextInt(n)



//val impure: () => Int = () => Random().nextInt(10)
val impure: Random ?=> () => Int = () => Random.nextInt(10)


@capability /* erased */ class Console:
  inline def println(x: String): Unit = Predef.println(x)

object Console:
  inline def println(x: String)(using Console):Unit = summon[Console].println(x)

// would be faster? and more compatible everything else written in Scala/Java with "type descriptors""
//
// type Println = Console ?-> String -> Unit
// overlay println(x: Any):Unit with Println


def somethingImpure(): (Console,Random) ?=> Int =
  Console.println("toto")
  impure()



type Task[T] = Ox ?-> T
type TaskWithState[T] = (Ox, State) ?-> T

extension [T](f: Ox ?=> T)
  def parN(times: Int): Ox ?=> Seq[T] =
    Seq.fill(times)(f.fork).map(_.join())


@main def hello =
  //Task need the '^' to compile, because State is this capture for later use //It's awesome, good work CC!
  def somejob()(using State): Task[(Int, Int)]^ = something()
  ///                                       ^^^^^ scalameta doesn't like T^ = // it's red in vscode
  
  //explicitly tracking st
  def somejob2()(using st:State): Task[(Int, Int)]^{st} = something()

  val withState: TaskWithState[(Int, Int)] = somejob()

  val impureWithState: (Console, Random) ?-> TaskWithState[Int]^ = somethingImpure()
  val impureWithtate2: (Console, Random, Ox, State) ?-> Int = somethingImpure()
  /**
    * the compiler is not able to expand yet (Console, Random) ?-> TaskWithState[Int] to 
    *                                        (Console, Random, Ox, State) ?-> Int
    * 
    * [error] Found:    (contextual$8: Console^, contextual$9: Random^) ?->?
    * [error]   (x$0: ox.Ox^?, x$1: State^?) ?->{contextual$8, contextual$9} Int
    * [error] Required: (contextual$8: Console^, contextual$9: Random^) ?-> TaskWithState[Int]
    * [error]   val impureWithState: (Console, Random) ?-> TaskWithState[Int] = somethingImpure()
    */
  supervised {
    given State = new State()

    val result = somejob()
    println(result)

    println(somejob().parN(16))
    //Extending functionality based on the inferred type Ox ?=> (Int, Int) is too generic.
    //would be nice to be able to call parN on things that are defined (using Ox)
    //it could create to much noise for method completion.
    println(result.parN(16))
  }

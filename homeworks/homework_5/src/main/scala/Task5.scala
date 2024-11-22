import scala.util.{Failure, Success, Try}

/*
  Задание №5
  Задание аналогично предыдущему задания, но теперь мы уходим от использования стандартного Either.
  Нужно:
  1) Доделать реализацию MyEither (нужны аналоги Right и Left)
  2) Написать для MyEither инстанс MonadError
  3) Написать функции apply, error, possibleError
 */
object Task5 extends App {
  import Task4.MonadError

  sealed trait MyEither[+E, +A] {
    def isError: Boolean
  }

  object MyEither {
    case class MyLeft[E](error: E) extends MyEither[E, Nothing] {
      override def isError: Boolean = true
    }

    case class MyRight[A](value: A) extends MyEither[Nothing, A] {
      override def isError: Boolean = false
    }

    def apply[A](value: A): MyEither[Nothing, A] = MyRight(value)

    def error[E, A](error: E): MyEither[E, A] = MyLeft(error)

    def possibleError[A](f: => A): MyEither[Throwable, A] = {
      Try(f) match {
        case Success(a) => MyRight(a)
        case Failure(e) => MyLeft(e)
      }
    }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      override def pure[A](x: A): MyEither[E, A] = MyRight(x)

      override def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] = fa match {
        case MyRight(a) => f(a)
        case MyLeft(e)  => MyLeft(e)
      }

      override def tailRecM[A, B](a: A)(f: A => MyEither[E, Either[A, B]]): MyEither[E, B] = f(a) match {
        case MyLeft(e)            => MyLeft(e)
        case MyRight(Left(nextA)) => tailRecM(nextA)(f)
        case MyRight(Right(b))    => MyRight(b)
      }

      override def raiseError[A](error: => E): MyEither[E, A] = MyLeft(error)

      override def handleErrorWith[A](fa: MyEither[E, A])(f: E => MyEither[E, A]): MyEither[E, A] = fa match {
        case MyLeft(e) => f(e)
        case r @ MyRight(_) => r
      }

      override def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] = fa match {
        case MyLeft(e) => MyRight(handle(e))
        case r @ MyRight(_) => r
      }
    }
  }

  object MyEitherSyntax {
    implicit class MyEitherOps[E, A](val either: MyEither[E, A]) {
      def flatMap[B](f: A => MyEither[E, B]): MyEither[E, B] =
        MyEither.myEitherMonad[E].flatMap(either)(f)

      def map[B](f: A => B): MyEither[E, B] = MyEither.myEitherMonad.map(either)(f)

      def handleError(f: E => A): MyEither[E, A] =
        MyEither.myEitherMonad.handleError(either)(f)
    }
  }
}

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
    final case class Right[+A](value: A) extends MyEither[Nothing, A] {
      override def isError: Boolean = false
    }

    final case class Left[+E](error: E) extends MyEither[E, Nothing] {
      override def isError: Boolean = true
    }

    def apply[A](value: A): MyEither[Nothing, A] = Right(value)
    def error[E, A](e: E): MyEither[E, A] = Left(e)
    def possibleError[A](f: => A): MyEither[Throwable, A] =
      Try(f) match {
        case Success(value) => Right(value)
        case Failure(error) => Left(error)
      }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      override def pure[A](value: A): MyEither[E, A] = MyEither(value)

      override def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] =
        fa match {
          case Right(value) => f(value)
          case Left(error) => Left(error)
        }

      override def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] = Left(error)

      override def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] =
        fa match {
          case Left(error) => Right(handle(error))
          case Right(value) => Right(value)
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

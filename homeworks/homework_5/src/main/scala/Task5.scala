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

	case class MyEitherRight[+A](value: A) extends MyEither[Nothing, A] {
		def isError: Boolean = false
	}

	case class MyEitherLeft[+E](error: E) extends MyEither[E, Nothing] {
		def isError: Boolean = true
	}

	object MyEither {
		def apply[A](value: A): MyEither[Nothing, A] = MyEitherRight(value)

		def error[E, A](error: E): MyEither[E, A] = MyEitherLeft(error)

		def possibleError[A](f: => A): MyEither[Throwable, A] = {
			try {
				MyEitherRight(f)
			} catch {
				case ex: Throwable => MyEitherLeft(ex)
			}
		}

		implicit def MyEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
			def pure[A](value: A): MyEither[E, A] = MyEitherRight(value)

			def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] =
				fa match {
					case MyEitherRight(a) => f(a)
					case MyEitherLeft(e) => MyEitherLeft(e)
				}

			override def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] = MyEitherLeft(error)

			def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] =
				fa match {
					case MyEitherRight(a) => MyEitherRight(a)
					case MyEitherLeft(e) => MyEitherRight(handle(e))
				}

		}
	}

	object MyEitherSyntax {
		implicit class MyEitherOps[E, A](val either: MyEither[E, A]) {
			def flatMap[B](f: A => MyEither[E, B]): MyEither[E, B] =
				MyEither.MyEitherMonad[E].flatMap(either)(f)

			def map[B](f: A => B): MyEither[E, B] = MyEither.MyEitherMonad.map(either)(f)

			def handleError(f: E => A): MyEither[E, A] =
				MyEither.MyEitherMonad.handleError(either)(f)
		}
	}
}

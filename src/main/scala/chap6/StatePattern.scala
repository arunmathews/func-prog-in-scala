package scala.chap6

/**
 * Implementation of state pattern
 */
object StatePattern {
  case class State[S, +A](run: S => (A, S)) {

    //Ex 10
    def mapWithoutFlatMap[B](f: A => B): State[S, B] = State(s => {
      val (a, s1) = run(s)

      (f(a), s1)
    })

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)

      f(a).run(s1)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(unit[S, List[A]](List()))((sa, sAccum) => sa.map2(sAccum)((a, accum) => a :: accum))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: => S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def modify2[S](f: S => S): State[S, Unit] = get.flatMap(s => set(f(s)).map(_ => ()))
  }
}


object Extensions:

  extension [A](set: Set[A])
    def containsAny(other: Set[A]): Boolean =
      set.exists(other.contains)

    def containsExactly(other: Set[A], amount: Int): Boolean =
      set.count(other.contains) == amount

  extension [A](list: List[A])
    def tailOrEmpty: List[A] =
      if list.isEmpty then List.empty else list.tail

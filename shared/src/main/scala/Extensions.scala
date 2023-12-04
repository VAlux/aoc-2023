object Extensions:

  extension [A](set: Set[A])
    def containsAny(other: Set[A]): Boolean =
      set.exists(other.contains)

    def containsExactly(other: Set[A], amount: Int): Boolean =
      set.count(other.contains) == amount

  extension (list: List[_])
    def tailOrEmpty: List[_] =
      if list.isEmpty then List.empty else list.tail

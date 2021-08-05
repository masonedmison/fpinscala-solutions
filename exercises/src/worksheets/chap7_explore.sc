def sum(ints: IndexedSeq[Int]): Int =
  if (ints.size <= 1)
    ints.headOption.getOrElse(0)
  else{
    val (l, r) = ints.splitAt(ints.size/2)
  }

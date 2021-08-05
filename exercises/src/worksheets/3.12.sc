def revList[A](l : List[A]): List[A] =
  l.foldLeft(Nil: List[A])((l: List[A], y: A) => y :: l)

val l1 = List(1,2,3)
revList(l1)
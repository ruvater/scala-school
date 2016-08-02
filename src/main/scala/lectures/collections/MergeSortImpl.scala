package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = data match {
    case head :: Nil => Seq(head)
    case _ => {
      val seqs = data.splitAt(data.length / 2)
      merge(mergeSort(seqs._1), mergeSort(seqs._2))
    }
  }

  def merge(first: Seq[Int], second: Seq[Int]): Seq[Int] = (first, second) match {
    case (seq, Nil) => seq
    case (Nil, seq) => seq
    case (h1 :: t1, h2 :: t2) if (h1 <= h2) => h1 +: merge(t1, second)
    case (seq, h2 :: t2) => h2 +: merge(first, t2)
  }

  require(mergeSort(List()) == List())
  require(mergeSort(List(1)) == List(1))
  require(mergeSort(List(2,1)) == List(1,2))
  require(mergeSort(List(1,1,5,4,3,2)) == List(1,1,2,3,4,5))
}

package forcomp

import common._

object Anagrams {

  /** ����� - ����� ���� `String`. */
  type Word = String

  /** ����������� - ��� ������ `List` ����. */
  type Sentence = List[Word]

  /** `Occurrences` ��� `List` ��� �������� � ������������� �����, ������� ���������� ������� ���������.
   *  ������ ������������ �� �������� ������� � ����.
   *  ��� ������� � ������ ��������.
   *
   *  ����� ������ ���, ������� � ������ ��������, �� �� ������������ **��** �������� ������� ���������
   *  
   *
   *  NB: ���� ������� ������� ����� 0, �� ���� ������ �� ������ �������������� � ������
   */
  type Occurrences = List[(Char, Int)]

  /** ������� - ��� ������ ������ ����
   *  ���� ���������������� �����-������� ��� �������� �������, `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** ������������ ����� � ������ ��������� ��������.
   *
   *  NB: ������� ��������������� ��� ����� �������� � ���������� � ������� � ������ ���������.
   *
   *  NB: �� ������� ������������ `groupBy` ��� ���������� ����� ������!
   */
  def wordOccurrences(w: Word): Occurrences =
  w.groupBy((c: Char) => c.toLower).toList.map((p: (Char, String)) => (p._1, p._2.length)).sortWith(_._1 < _._1)

  /** ������������ ����������� � ������ ��������� ��� ��������. */
  def sentenceOccurrences(s: Sentence): Occurrences = if (s.isEmpty) List() else wordOccurrences(s.reduce(_ concat _))

  /** `dictionaryByOccurrences` ���� `Map` �� ��������� ������� ��������� � ������������������ ���� ����,
   *  ������� ����� ��� ����� ���������.
   *  ���� ��� ������������� ������� ������ ��������� ���� �������� �����.
   *
   *  ��������, ����� "eat" ����� ��������� ������ ���������:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  ��� � ����� "ate" � "tea".
   *
   *  ��� ��������, ��� `dictionaryByOccurrences` ����� ��������� ������:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  dictionary.groupBy(wordOccurrences(_))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
  (dictionaryByOccurrences withDefaultValue List())(wordOccurrences(word))

  /** ���������� ������ ���� ����������� ������ ���������.
   *  �������� ���� ��������� `List(('k', 1), ('o', 1))`
   *  �������� ������������� `List(('k', 1), ('o', 1))`.
   *  ����� �������� ������ ������������ `List()`.
   *
   *  ������: ������������ ������ ��������� `List(('a', 2), ('b', 2))` ���������:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  ������� �� ����� -- ������������ � ������� ���� ����� ���� � ������ �������.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =
  if (occurrences.isEmpty) List(List())
  else {
    val restOccurrences: List[Occurrences] = combinations(occurrences.tail)
    val currentSymbol = occurrences.head._1
    val currentSymbolFreq = occurrences.head._2
    (for {
      i <- 0 until (currentSymbolFreq + 1)
      os <- restOccurrences
    } yield if (i == 0) os else (currentSymbol, i) :: os).toList
  }

  /** �������� ������ ��������� `y` �� ������ ��������� `x`.
   *
   *  ����������� - �� ��� `y` �������� ������������� `x` -- ����� ������ �������� � `y`������ ������� � `x`, � ��� ������� ������ ���� ������ ���� �����
   *  ������� � `x`.
   *
   *  NB: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
  if (y.isEmpty) x
  else {
    val (first, rest) = x.span((p: (Char, Int)) => p._1 != y.head._1)
    val newFrequency = rest.head._2 - y.head._2
    val newRest = if (newFrequency > 0) (rest.head._1, newFrequency) :: rest.tail else rest.tail
    subtract(first ::: newRest, y.tail)
  }

  /** ���������� ������ ���� �������� �����������.
   *
   *  ��������� ����������� ����������� �� ������ ��������� ���� ��������
   *  ���� ���� ����������� � ���������� ��� ��������� ���������� ���� � ����� �������,
   *  ��� ��� ����� ��������� � �������.
   *
   *  ����� ���� � ����������� � ��� ��������� �� ������� ���������.
   *  ��������, ����������� `List("I", "love", "you")` ��������� ����������� `List("You", "olive")`.
   *
   *  �����, ����������� � ���� �� �������, ���  ������ ������� �������� ����� ���������� �����������.
   *  ��������, ����������� `List("You", "olive")` � `List("olive", "you")` ��������� ���������
   *  `List("I", "love", "you")`.
   *
   *  ����� ������ ������ ��� ����������� `List("Yes", "man")` � ��� �������� �� �������:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  ��������� ����������� �� ������� ���� � ��������� ������� - ����� ������� ��������, ���� ��� ��������� ������ � ���������.
   *  ������ ������������ ����� ������ �������������� � �������.
   *
   *  NB: � ������, ���� ����� � ����������� �� �������, �� ����������� �������� ���������� ������ ����, ������� ��� ������ �������������� � ���� ������.
   *
   *  NB: ���� ������ ���� ��������� ������� �����������.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagrams0(occs: Occurrences): List[Sentence] =
      if (occs.isEmpty) List(List())
      else (for {
        comb <- combinations(occs)
        word <- (dictionaryByOccurrences withDefaultValue List())(comb)
        restSentence <- sentenceAnagrams0(subtract(occs, comb))
      } yield word :: restSentence).toList

    sentenceAnagrams0(sentenceOccurrences(sentence))
  }
}

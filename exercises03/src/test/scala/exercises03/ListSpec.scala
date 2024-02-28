package exercises03

class ListSpec extends org.scalatest.wordspec.AnyWordSpec {
  "MyList.sum" should {
    "sum list" in {
      val list = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))

      assert(MyList.sum(list) == 10)
    }

    "sum Nil" in {
      val list = Nil

      assert(MyList.sum(list) == 0)
    }
  }

  "MyList.reverse" should {
    "reverse list" in {
      val list = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))

      assert(MyList.reverse(list) == Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    }

    "reverse Nil" in {
      val list = Nil

      assert(MyList.reverse(list) == Nil)
    }
  }

  "MyList.foldLeft" should {
    "fold sum" in {
      val list4         = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
      val list5         = Cons(5, list4)
      val folding4: Int = MyList.foldLeft(list4)(0)((a, b) => a + b)
      val folding5: Int = MyList.foldLeft(list5)(0)((a, b) => a + b)

      assert(folding4 == 10)
      assert(folding5 == 15)
    }

    "fold minus" in {
      val list4         = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
      val list5         = Cons(5, list4)
      val folding4: Int = MyList.foldLeft(list4)(0)((a, b) => a - b)
      val folding5: Int = MyList.foldLeft(list5)(0)((a, b) => a - b)

      assert(folding4 == -10)
      assert(folding5 == -15)
    }
  }

  "MyList.last" should {
    "last" in {
      val list = Cons(77, Cons(3, Cons(2, Cons(99, Nil))))

      assert(MyList.last(list).contains(99))
    }

    "last nil" in {
      assert(MyList.last(Nil).isEmpty)
    }
  }

  "MyList.size" should {
    "size" in {
      val list = Cons(77, Cons(3, Cons(2, Cons(1, Nil))))

      assert(MyList.size(list) == 4)
    }

    "size nil" in {

      assert(MyList.size(Nil) == 0)
    }
  }

  "MyList.max" should {
    "max" in {
      val list = Cons(77, Cons(3, Cons(88, Cons(1, Nil))))

      assert(MyList.max[Int](list, _ > _).contains(88))
    }

    "max nil" in {

      assert(MyList.max[Int](Nil, _ > _).isEmpty)
    }
  }

  "MyList.filter" should {
    "filter even" in {
      val list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      assert(
        MyList
          .filter[Int](list, element => if (element % 2 == 0) MyList.Filter.Preserve else MyList.Filter.Skip) == Cons(
          2,
          Cons(4, Nil)
        )
      )
    }

    "filter drops all" in {
      val list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      assert(MyList.filter[Int](list, _ => MyList.Filter.Skip) == Nil)
    }

    "filter preserve all" in {
      val list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      assert(MyList.filter[Int](list, _ => MyList.Filter.Preserve) == list)
    }

    "filter nil preserve" in {

      assert(MyList.filter[Int](Nil, _ => MyList.Filter.Preserve) == Nil)
    }
  }
}

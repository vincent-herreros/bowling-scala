package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {
    describe("score") {
    describe("when score is ok") {
      it("should have size 0") {
        val tir=tirGeneration(0, 10, List(), 0, false)
        assert(Bowling.score(tir.get, 0, 0) == 0)
      }
      it("should have size 20"){
        val tir=tirGeneration(1, 10, List(), 0, false)
        assert(Bowling.score(tir.get, 0, 0) == 20)
      }
      it("should have size 300"){
          val tir=tirGeneration(10, 10, List(), 2, false)
          assert(Bowling.score(tir.get, 0, 0) == 300)
      }

      it("should produce NoSuchElementException when head is invoked") {
        assertThrows[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
    describe("when roll have a good number"){
      it("should have a shoot between 0 and 10 with frame random with i<5"){
        val f=Frame(4, 0, true, 4)
        assert(f.get.tir(0)<=10 && f.get.tir(0)>=0 && f.get.tir(1)<=10 && f.get.tir(1)>=0)
      }

      it("should have a shoot between 0 and 10 with frame random with i>5"){
        val f=Frame(7, 0, true, 4)
        assert(f.get.tir(0)<=10 && f.get.tir(0)>=0 && f.get.tir(1)<=10 && f.get.tir(1)>=0)
      }

      it("should have a shoot between 0 and 10 with frame with strike"){
        val f=Frame(10, 2, false, 4)
        assert(f.get.tir(0)<=10 && f.get.tir(0)>=0 && f.get.tir(1)==0)
      }

      it("should have a shoot between 0 and 10 with frame with spare"){
        val f=Frame(7, 1, false, 4)
        assert(f.get.tir(0)<=10 && f.get.tir(0)>=0 && f.get.tir(1)<=10 && f.get.tir(1)>=0 && f.get.tir(0)+f.get.tir(1)==10)
      }
    }
  }


  def tirGeneration(i: Int, iteration: Int, currentList: List[Frame], strike: Int, random: Boolean): Option[List[Frame]]={
      if(iteration==0){
          Some(currentList)
      }
      else{
        val f =Frame(i, strike, random, iteration)
        if (f.isEmpty){
            None
        }
        else{
            tirGeneration(i, iteration-1, f.get::currentList, strike, random)
        }
      }
  }
}

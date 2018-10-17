package bowling
import scala.util.Random

object Bowling extends App{
    def score(frame: List[Frame], currentScore: Int, strike: Int): Int = {
        frame match{
            case Nil => {
                if(currentScore>300){
                    300
                }
                else{
                    currentScore
                }
            }
            case a::Nil => {
                if(a.tir(0)!=10){
                    score(Nil,currentScore+a.tir(0)+a.tir(1), 0)
                }
                else{
                    if(a.tir(1)!=10){
                        score(Nil,currentScore+10+2*a.tir(1)+a.tir(2), 0)
                    }
                    else{
                        score(Nil, currentScore+3*10+3*a.tir(2), 0)
                    }
                }
            }
            case a::b => {
                if(a.tir(0)!=10){
                    if(strike==2){
                        if(a.tir(0)+a.tir(1)!=10){
                            score(b ,currentScore+10+2*(a.tir(0)+a.tir(1)), 0)
                        }
                        else{
                            score(b ,currentScore+10+2*(a.tir(0)+a.tir(1)), 1)
                        }
                    }
                    else{
                        score(b, currentScore+a.tir(0)+a.tir(1), 0)
                    }
                }
                else if(a.tir(0)+a.tir(1)==10 && a.tir(0)!=10){
                    score(b, currentScore, 0)
                }
                else{
                    val s = strikef(b)
                    return score(b, currentScore+s, 2)
                }
            }
        }
    }

    def strikef(shoot: List[Frame]): Int = {
        if(shoot.length>=2){
            if(shoot.head.tir(0)==10){
                20+shoot.tail.head.tir(0)
            }
            else{
                10+shoot.head.tir(0)+shoot.head.tir(1)
            }
        }
        else if(shoot.length==1){
            10+shoot.head.tir(0)+shoot.head.tir(1)
        }
        else{
            10
        }
    }

}

case class Frame(val tir:List[Int]){
    
}
object Frame{
    def apply(i: Int, strike: Int, random: Boolean, numberFrame: Int): Option[Frame] = {
        if(i>10){
            None
        }
        else{
            if(numberFrame==10){
                if(strike==2){
                    Some(new Frame(List(10,10,10)))
                }
                else if(strike ==1){
                    Some(new Frame(List(i, 10-i, (new Random().nextInt(11)))))
                }
                else{
                    Some(new Frame(List(i, i)))
                }
            }
            else{
                if(strike==2){
                    Some(new Frame(List(10, 0)))
                }
                else if(strike==1){
                    Some(new Frame(List(i, 10-i)))
                }
                else{
                    if(random){
                        Some(new Frame(List(i, (new Random()).nextInt(10-i))))
                    }
                    else{
                        Some(new Frame(List(i, i)))
                    }
                }
            }
        }
    }
}



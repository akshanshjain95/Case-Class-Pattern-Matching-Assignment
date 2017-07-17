import com.knoldus.kip.models.{Scoreboard, Student, Subject}

case class Demo(lol: String)

def expressionRevisited: PartialFunction[Any, String] = {
  case x: Student => "Shut up "
  case x: Subject => "Hmmm .... "
  case x: Scoreboard => "aha "
  case x: Demo => "Demooo"
  case _ => "!!! ???"
}


val a = Demo("Akshansh")
expressionRevisited(a)

expressionRevisited.isDefinedAt(Demo("l"))
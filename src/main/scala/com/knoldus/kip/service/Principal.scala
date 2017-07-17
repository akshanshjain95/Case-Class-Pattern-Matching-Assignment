package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models._

trait Principal {


  def findOutIfCSE(id: Int): CoursePerformance = {

    val receiveCoursePerformance = RamDatabase.getById(id)
    val courseName = receiveCoursePerformance.map(_.course.name).getOrElse("None")
    courseName match {
      case "CSE" => receiveCoursePerformance.get
      case _ => throw new Exception

    }
  }

  def findOutIfAnyCourse(id: Int, courseName: String): CoursePerformance = {

    val receiveCoursePerformance = RamDatabase.getById(id)
    val receiveCourseName = receiveCoursePerformance.map(_.course.name).getOrElse("None")
    receiveCourseName match {
      case courseName => receiveCoursePerformance.get
      case _ => throw new Exception
    }
  }

  def expression(mod: Any): String = {

    mod match {
      case x: Student => "Shut up "
      case x: Subject => "Hmmm .... "
      case x: Scoreboard => "aha "
      case _ => "!!! ???"
    }
  }

/*  def checkScoreboard(scoreboard: Scoreboard): List[String] = {


  }*/

  def expressionRevisited: PartialFunction[ModelIdentifier, String] = {
    case x: Student => "Shut up "
    case x: Subject => "Hmmm .... "
    case x: Scoreboard => "aha "
    case _ => "!!! ???"
  }

}

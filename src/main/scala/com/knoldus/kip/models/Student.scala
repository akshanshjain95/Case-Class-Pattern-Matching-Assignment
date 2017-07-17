package com.knoldus.kip.models

case class Student(id: Int, firstName: String, middleName: Option[String], lastName: String,
                   rollNumber: Int, age: Option[Int], gender: Char, enrollmentNumber: Long,
                  address: Option[String]) extends ModelIdentifier{

  def getAddress: String = address.fold("N/A"){i => i}
  def getMiddleName: String = middleName.map(_.split(" ").head).getOrElse("N/A")

}

case class Subject(id:Int, subjectName: String, obtainedMarks:Float,
                   maxMarks: Float) extends ModelIdentifier

case class Course(id: Int, name: String, category: String,
                  subjects: List[Subject]) extends ModelIdentifier


case class Scoreboard(id: Int, student: Student, subjects: List[Subject], total: Float,
                      percentage: Float, grade: String) extends ModelIdentifier {

  def getHighestScore: List[Subject] = {
    val listOfMarks = subjects.map(_.obtainedMarks)
    val highestScore = listOfMarks.max
    subjects.filter(_.obtainedMarks == highestScore)
  }

  def getLowestScore: List[Subject] = {
    val listOfMarks = subjects.map(_.obtainedMarks)
    val lowestScore = listOfMarks.min
    subjects.filter(_.obtainedMarks == lowestScore)
  }

}

object Scoreboard {
  def apply(student: Student, subjects: List[Subject]): Scoreboard = {
    val total = subjects.map{_.obtainedMarks}.sum
    val percentage = (total/subjects.map{_.maxMarks}.sum)
    val grade = if(percentage >= 95){
      "A+"
    }
    else if(percentage >= 90)
    {
      "A"
    }
    else if(percentage >= 85)
    {
      "B+"
    }
    else if(percentage >= 80)
    {
      "B"
    }
    else if(percentage >= 70)
    {
      "C+"
    }
    else if(percentage >= 60)
    {
      "C"
    }
    else if(percentage >= 50)
    {
      "D+"
    }
    else if(percentage >= 40)
    {
      "D"
    }
    else
    {
      "F"
    }
    new Scoreboard(student.id, student, subjects, total,
      percentage, grade)
  }

  def checkScoreboard(scorecards: List[Scoreboard]): List[String] = {
    for{
      scoreboard <- scorecards
      highestScore <- scoreboard.getHighestScore
    } yield {scoreboard.student.firstName + " " + highestScore.subjectName + " " + highestScore.obtainedMarks}
  }

}

case class CoursePerformance(id: Int, year: Int, course: Course,
                             scoreCards: List[Scoreboard]) extends ModelIdentifier
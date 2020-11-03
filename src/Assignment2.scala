object Assignment2 {

  def main(args: Array[String]): Unit = {

    println("Task 1")
    println(daysPatternMatch("Monday"))
    println(daysPatternMatch("Wednesday"))
    println(daysPatternMatch("FRIDAY"))
    println(daysPatternMatch("sunday"))
    println(daysPatternMatch("sandwich"))

    println("\nTask 2")
    val acc = new BankAccount(250000.35)
    println("account balance = " + acc.balance)
    acc.deposit(350)
    println("account balance = " + acc.balance)
    val acc1 = new BankAccount()
    println("account 1 balance = " + acc1.balance)

    println("\nTask 3")
    val person1 = new Person("Mike", "Morhaime")
    val person2 = new Person("Jill", "Smith")
    val person3 = new Person("John", "Doe")
    val person4 = new Person("Cave", "Johnson")
    println(getGreeting(person1))
    println(getGreeting(person2))
    println(getGreeting(person3))
    println(getGreeting(person4))

    println("\nTask 4")
    println(tripleApplication(checkTheSign, -6))

    println("\nTask 5")
    val person = new Person("John", "Smith")
    val employee = new Person("Jake", "Johnson") with Employee {
      override var salary: Double = 15000
    }

    val student = new Person("Swen", "Olafsson") with Student {
    }

    val teacher = new Person("Gurney", "Halleck") with Teacher {
      override var salary: Double = 12000
    }

    val workingStudent = new Person("Matti", "Hendricks") with Student with Employee {
      override var salary: Double = 10000
    }

    val studyingEmployee = new Person("Poe", "Dameron") with Employee with Student {
      override var salary: Double = 18000
    }

    println("Generic person: " + person)
    println("Employee: "+ employee)
    println("Student: " + student)
    println("Teacher: " + teacher)
    println("Student employee: " + workingStudent)
    println("Employee who also is a student: " + studyingEmployee)
  }
    def daysPatternMatch(string: String): String = string.toLowerCase match {
      case "monday" | "tuesday" | "wednesday" | "thursday" | "friday" => "work"
      case "saturday" | "sunday" => "weekend"
      case _ => "no such day"
    }

    def getGreeting(person: Person): String = person.firstName match {
      case "Mike" => "Good day, sir!"
      case "Jill" => "Madam, nice to see you"
      case _ => "Hey there!"
    }


  class BankAccount(val currentBalance: Double) {
    private var _balance = currentBalance

    def deposit(sum: Double) {
      _balance = _balance + sum
    }

    def withdraw(sum: Double) {
      _balance = _balance - sum
    }

    def balance: Double = _balance

    def this() {
      this(0)
    }
  }

  def tripleApplication(f: Int => Int, n : Int) : Int = f(f(f(n)))
  def checkTheSign(n: Int) : Int = if(n > 0 ) n * 2 else n - 6

  class Person(val firstName: String, val lastName: String, private val tax: Double) {
    def taxToPay: Double = tax

    def this(firstName: String, lastName: String) {
      this(firstName, lastName, 0)
    }

    override def toString: String = "Name - " + firstName + ", last name - " + lastName
  }

  trait Student extends Person {
    override def taxToPay: Double = 0
    override def toString: String = super.toString + ", Tax: " + taxToPay
  }

  trait Employee extends Person {
    var salary: Double
    override def taxToPay: Double = salary * 0.2
    override def toString: String = super.toString + ". Salary: " + salary + ", Tax: " + taxToPay
  }

  trait Teacher extends Person with Employee {
    override def taxToPay: Double = salary * 0.1
  }
}

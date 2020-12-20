object Assignment2 {

  def main(args: Array[String]): Unit = {

    /*1. Use pattern matching in a function accepting a String.
    For strings containing names of days of the week return “work” (Monday-Friday)
    or “weekend” ( for weekends).
    For all other strings return “no such day”*/
    println("Task 1")
    println(daysPatternMatch("Monday"))
    println(daysPatternMatch("Wednesday"))
    println(daysPatternMatch("FRIDAY"))
    println(daysPatternMatch("sunday"))
    println(daysPatternMatch("sandwich"))

    /*2. Define a BankAccount class with methods deposits and withdraw and currentBalance read only property.
    The class should provide a constructor accepting initial balance
    and second constructor, setting the balance to 0.*/
    println("\nTask 2")
    val acc = new BankAccount(250000.35)
    println("account balance = " + acc.balance)
    acc.deposit(350)
    println("account balance = " + acc.balance)
    val acc1 = new BankAccount()
    println("account 1 balance = " + acc1.balance)

    /*3. Define a Person class with properties firstName and lastName.
    Create a few instances. Define a function accepting Person instance
    and using pattern matching to select string greeting the person.
    Define 2-3 different greetings for people meeting specific criteria
    (eg. specific first or last name) and one generic greeting*/
    println("\nTask 3")
    val person1 = new Person("Mike", "Morhaime")
    val person2 = new Person("Jill", "Smith")
    val person3 = new Person("John", "Doe")
    val person4 = new Person("Cave", "Johnson")
    println(getGreeting(person1))
    println(getGreeting(person2))
    println(getGreeting(person3))
    println(getGreeting(person4))

    /*4. Define a function accepting two parameters –
    integer and a function operating on integers.
    Apply the parameter function three times to the integer and return a result*/
    println("\nTask 4")
    println(tripleApplication(checkTheSign, -6))

    /*5. Define a Person class and three traits: Student, Teacher, Employee.
    Person should have firstName, lastName and taxToPay read only properties.
    Employee should have a salary property (with getter and setter).
    Student and Employee traits should override taxToPay property –
    for Student it should return 0, for Employee it should return 20% of their salary.
    Teacher should inherit from employee, the taxToPay should return 10% of their salary.
    Create objects with each of the properites, show how their properties work.
    Create two objects with both the Student and Employee properties
    (add them in different order in each object), show how taxToPay will work with this object
    depending on the order those traits are added upon instantiation.*/
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

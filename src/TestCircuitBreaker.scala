import org.circuitbreaker._
import org.specs.Specification

/**
 * User: FaKod
 * Date: 23.03.2010
 * Time: 08:31:43
 */


class Test extends UsingCircuitBreaker {

  def myMethod = {
    withCircuitBreaker("test") {
    }
  }

  def my2Method:Long = {
    withCircuitBreaker("test") {
      throw new java.lang.IllegalArgumentException
    }
  }

}

import CircuitBreaker._
object TestCircuitBreaker extends Specification {

  "A CircuitBreaker" should {
    setSequential()
    shareVariables()

    addCircuitBreaker("test", CircuitBreakerConfiguration(100,10))
    addCircuitBreaker("test2", CircuitBreakerConfiguration(100,10))

    "remain closed" in {
      for(i <- 1 to 20) {
        new Test().myMethod
      }
    }

    "be half open" in {
      for(i <- 1 to 10) {
        println("1: " + i)
        (new Test().my2Method) must throwA[java.lang.IllegalArgumentException]
      }

      for(i <- 1 to 10) {
        println("2: " + i)
        (new Test().my2Method) must throwA[CircuitBreakerOpenException]
      }

      Thread.sleep(200)

      (new Test().my2Method) must throwA[CircuitBreakerHalfOpenException]

      for(i <- 1 to 10) {
        println("3: " + i)
        (new Test().my2Method) must throwA[CircuitBreakerOpenException]
      }
    }

  }
}
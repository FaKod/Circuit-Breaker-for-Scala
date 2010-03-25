package org.circuitbreaker

import collection.immutable.HashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference, AtomicInteger}
import System._

/**
 *
 */
object CircuitBreaker {
  private var circuitBreaker = HashMap[String, CircuitBreaker]()

  def addCircuitBreaker(name: String, config: CircuitBreakerConfiguration): Unit = {
    circuitBreaker.get(name) match {
      case None => circuitBreaker += ((name, new CircuitBreakerImpl(config)))
      case Some(x) => throw new java.lang.IllegalArgumentException("CircuitBreaker " + name + " already configured")
    }

  }

  private[circuitbreaker] def apply(name: String): CircuitBreaker = {
    circuitBreaker.get(name) match {
      case Some(x) => x
      case None => throw new java.lang.IllegalArgumentException("CircuitBreaker " + name + " not configured")
    }
  }
}

/**
 *
 */
trait UsingCircuitBreaker {
  def withCircuitBreaker[T](name: String)(f: => T): T = {
    CircuitBreaker(name).invoke(f)
  }
}

/**
 *
 */
case class CircuitBreakerConfiguration(timeout: Long, failureThreshold: Int)

/**
 *
 */

private[circuitbreaker] trait CircuitBreaker {
  var failureCount: Int

  var tripTime: Long

  def invoke[T](f: => T): T

  def trip

  def resetFailureCount

  def attemptReset

  def reset

  def failureThreshold: Int

  def timeout: Long
}

/**
 *
 */

private[circuitbreaker] abstract class CircuitBreakerBase(config: CircuitBreakerConfiguration) {
  /**
   * base class private members
   */
  private var _state = new AtomicReference[States]

  private var _failureThreshold = new AtomicInteger(config.failureThreshold)

  private var _timeout = new AtomicLong(config.timeout)

  private var _failureCount = new AtomicInteger(0)

  private var _tripTime = new AtomicLong

  /**
   * access members
   */

  protected def state_=(s: States) {_state.set(s)}

  protected def state = _state.get

  def failureThreshold = _failureThreshold get

  def timeout = _timeout get

  def failureCount_=(i: Int) {_failureCount.set(i)}

  def failureCount = _failureCount.incrementAndGet

  def tripTime_=(l: Long) {_tripTime.set(l)}

  def tripTime = _tripTime.get

}

/**
 *
 */
private[circuitbreaker] class CircuitBreakerImpl(config: CircuitBreakerConfiguration)
        extends CircuitBreakerBase(config)
                with CircuitBreaker {
  reset

  def reset = {
    resetFailureCount
    state = new ClosedState(this)
  }

  def resetFailureCount =
    failureCount = 0

  def attemptReset =
    state = new HalfOpenState(this)

  def trip = {
    tripTime = currentTimeMillis
    state = new OpenState(this)
  }

  def invoke[T](f: => T): T = {
    state.preInvoke
    try {
      val ret = f
      state.postInvoke
      ret
    }
    catch {
      case e: Throwable => {
        state.onError(e)
        throw e
      }
    }
  }
}

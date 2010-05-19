/*
 * Copyright Christopher Schmidt 2010
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.circuitbreaker

import collection.immutable.HashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference, AtomicInteger}
import System._

/**
 * holder companion object for creating and retrieving all
 * configured CircuitBreaker (CircuitBreaker) instances
 * (Enhancements could be to put some clever ThreadLocal stuff in here)
 *
 * @author Christopher Schmidt
 */
object CircuitBreaker {

  /**
   * holds instances
   */
  private var circuitBreaker = HashMap[String, CircuitBreaker]()

  /**
   * factory mathod
   * creates a new CircuitBreaker with a given name and configuration
   *
   * @param name name or id of the new CircuitBreaker
   * @param config CircuitBreakerConfiguration to configure the new CircuitBreaker
   */
  def addCircuitBreaker(name: String, config: CircuitBreakerConfiguration): Unit = {
    circuitBreaker.get(name) match {
      case None => circuitBreaker += ((name, new CircuitBreakerImpl(config)))
      case Some(x) => throw new java.lang.IllegalArgumentException("CircuitBreaker " + name + " already configured")
    }

  }

  /**
   * CircuitBreaker retrieve method
   *
   * @param name String name or id of the CircuitBreaker
   * @return CircuitBreaker with name or id name
   */
  private[circuitbreaker] def apply(name: String): CircuitBreaker = {
    circuitBreaker.get(name) match {
      case Some(x) => x
      case None => throw new java.lang.IllegalArgumentException("CircuitBreaker " + name + " not configured")
    }
  }
}

/**
 * Basic MixIn for using CircuitBreaker Scope method
 *
 * @author Christopher Schmidt
 */
trait UsingCircuitBreaker {
  def withCircuitBreaker[T](name: String)(f: => T): T = {
    CircuitBreaker(name).invoke(f)
  }
}


/**
 * simple case class that holds configuration parameter
 *
 * @param timeout timout for trying again
 * @param failureThreshold threshold of errors till breaker will open
 *
 * @author Christopher Schmidt
 */
case class CircuitBreakerConfiguration(timeout: Long, failureThreshold: Int)


/**
 * Interface definition for CircuitBreaker
 *
 * @author Christopher Schmidt
 */
private[circuitbreaker] trait CircuitBreaker {

  /**
   * increments and gets the actual failure count
   *
   * @return Int failure count
   */
  var failureCount: Int

  /**
   * @return Long milliseconds at trip
   */
  var tripTime: Long

  /**
   * function that has to be applied in CircuitBreaker scope
   */
  def invoke[T](f: => T): T

  /**
   * trip CircuitBreaker, store trip time
   */
  def trip

  /**
   * sets failure count to 0
   */
  def resetFailureCount

  /**
   * set state to Half Open
   */
  def attemptReset

  /**
   * reset CircuitBreaker to configured defaults
   */
  def reset

  /**
   * @return Int configured failure threshold
   */
  def failureThreshold: Int

  /**
   * @return Long configured timeout
   */
  def timeout: Long
}


/**
 * CircuitBreaker base class for all configuration things
 * holds all thread safe (atomic) private members
 *
 * @author Christopher Schmidt
 */
private[circuitbreaker] abstract class CircuitBreakerBase(config: CircuitBreakerConfiguration) extends CircuitBreaker {
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

  protected def state_=(s: States) {
    _state.set(s)
  }

  protected def state = _state.get

  def failureThreshold = _failureThreshold get

  def timeout = _timeout get

  def failureCount_=(i: Int) {
    _failureCount.set(i)
  }

  def failureCount = _failureCount.incrementAndGet

  def tripTime_=(l: Long) {
    _tripTime.set(l)
  }

  def tripTime = _tripTime.get

}

/**
 * CircuitBreaker implementation class for changing states
 *
 * @author Christopher Schmidt
 */
private[circuitbreaker] class CircuitBreakerImpl(config: CircuitBreakerConfiguration) extends CircuitBreakerBase(config)
{
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

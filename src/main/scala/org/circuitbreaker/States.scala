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

import System._

/**
 * CircuitBreaker states base class
 *
 * @author Christopher Schmidt
 */
private[circuitbreaker] abstract class States(cb: CircuitBreaker) {

  /**
   * pre invocation method
   */
  def preInvoke

  /**
   * post invocation method
   */
  def postInvoke

  /**
   * called if exception is thrown in applied function
   */
  def onError(e: Throwable)
}


/**
 * CircuitBreaker is closed, normal operation
 *
 * @author Christopher Schmidt
 */
private[circuitbreaker] class ClosedState(cb: CircuitBreaker) extends States(cb) {
  def onError(e: Throwable) = {
    val currentCount = cb.failureCount
    val threshold = cb.failureThreshold
    if (currentCount >= threshold)
      cb.trip
  }

  def postInvoke =
    cb.resetFailureCount

  def preInvoke = null
}


/**
 * CircuitBreaker is open. Calls are failing fast
 *
 * @author Christopher Schmidt
 */
private[circuitbreaker] class OpenState(cb: CircuitBreaker) extends States(cb) {
  def onError(e: Throwable) = null

  def postInvoke = null

  def preInvoke = {
    val now = currentTimeMillis
    val elapsed = now - cb.tripTime
    if (elapsed <= cb.timeout)
      throw new CircuitBreakerOpenException("Circuit Breaker is open; calls are failing fast")
    cb.attemptReset
  }
}


/**
 * CircuitBreaker is half open. Calls are still failing after timeout
 *
 * @author Christopher Schmidt
 */
private[circuitbreaker] class HalfOpenState(cb: CircuitBreaker) extends States(cb) {
  def onError(e: Throwable) = {
    cb.trip
    throw new CircuitBreakerHalfOpenException("Circuit Breaker is half open; calls are still failing after timout", e)
  }

  def postInvoke =
    cb.reset

  def preInvoke = null
}
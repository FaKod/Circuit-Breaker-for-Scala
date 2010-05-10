package org.circuitbreaker

import System._

/**
 *
 */
private[circuitbreaker] abstract class States(cb: CircuitBreaker) {
  def preInvoke

  def postInvoke

  def onError(e: Throwable)
}

/**
 *
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
 *
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
 *
 */
private[circuitbreaker] class HalfOpenState(cb: CircuitBreaker) extends States(cb) {
  def onError(e: Throwable) = {
    cb.trip
    throw new CircuitBreakerHalfOpenException("Circuit Breaker is half open; calls are still failing", e)
  }

  def postInvoke =
    cb.reset

  def preInvoke = null
}
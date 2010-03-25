package org.circuitbreaker


/**
 *
 */
abstract class CircuitBreakerException(message: String, cause: Throwable) extends Throwable(message, cause)
class CircuitBreakerOpenException(message: String, cause: Throwable = null) extends CircuitBreakerException(message, cause)
class CircuitBreakerHalfOpenException(message: String, cause: Throwable = null) extends CircuitBreakerException(message, cause)

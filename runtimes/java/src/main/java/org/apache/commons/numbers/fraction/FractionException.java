/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.numbers.fraction;

/**
 * Package private exception class with constants for frequently used messages.
 */
class FractionException extends ArithmeticException {

    /** Error message for overflow during conversion. */
    static final String ERROR_CONVERSION_OVERFLOW = "Overflow trying to convert %s to fraction (%d/%d)";
    /** Error message when iterative conversion fails. */
    static final String ERROR_CONVERSION = "Unable to convert %s to fraction after %d iterations";
    /** Error message for zero-valued denominator. */
    static final String ERROR_ZERO_DENOMINATOR = "Denominator must be different from 0";
    /** Error message for divide by zero. */
    static final String ERROR_DIVIDE_BY_ZERO = "The value to divide by must not be zero";

    /** Serializable version identifier. */
    private static final long serialVersionUID = 201701191744L;

    /**
     * Create an exception where the message is constructed by applying
     * {@link String#format(String, Object...)}.
     *
     * @param message  the exception message format string
     * @param formatArguments the arguments for formatting the message
     */
    FractionException(String message, Object... formatArguments) {
        super(String.format(message, formatArguments));
    }

    /**
     * Create an exception with the specified message.
     *
     * @param message  the exception message
     */
    FractionException(String message) {
        super(message);
    }
}

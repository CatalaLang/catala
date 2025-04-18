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

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Objects;
import org.apache.commons.numbers.core.NativeOperators;

/**
 * Representation of a rational number using arbitrary precision.
 *
 * <p>The number is expressed as the quotient {@code p/q} of two {@link BigInteger}s,
 * a numerator {@code p} and a non-zero denominator {@code q}.
 *
 * <p>This class is immutable.
 *
 * <a href="https://en.wikipedia.org/wiki/Rational_number">Rational number</a>
 */
public final class BigFraction
    extends Number
    implements Comparable<BigFraction>,
               NativeOperators<BigFraction>,
               Serializable {
    /** A fraction representing "0". */
    public static final BigFraction ZERO = new BigFraction(BigInteger.ZERO);

    /** A fraction representing "1". */
    public static final BigFraction ONE = new BigFraction(BigInteger.ONE);

    /** Serializable version identifier. */
    private static final long serialVersionUID = 20190701L;

    /** The default iterations used for convergence. */
    private static final int DEFAULT_MAX_ITERATIONS = 100;

    /** Message for non-finite input double argument to factory constructors. */
    private static final String NOT_FINITE = "Not finite: ";

    /** The overflow limit for conversion from a double (2^31). */
    private static final long OVERFLOW = 1L << 31;

    /** The numerator of this fraction reduced to lowest terms. */
    private final BigInteger numerator;

    /** The denominator of this fraction reduced to lowest terms. */
    private final BigInteger denominator;

    /**
     * Private constructor: Instances are created using factory methods.
     *
     * <p>This constructor should only be invoked when the fraction is known
     * to be non-zero; otherwise use {@link #ZERO}. This avoids creating
     * the zero representation {@code 0 / -1}.
     *
     * @param num Numerator, must not be {@code null}.
     * @param den Denominator, must not be {@code null}.
     * @throws ArithmeticException if the denominator is zero.
     */
    private BigFraction(BigInteger num, BigInteger den) {
        if (den.signum() == 0) {
            throw new FractionException(FractionException.ERROR_ZERO_DENOMINATOR);
        }

        // reduce numerator and denominator by greatest common denominator
        final BigInteger gcd = num.gcd(den);
        if (BigInteger.ONE.compareTo(gcd) < 0) {
            numerator = num.divide(gcd);
            denominator = den.divide(gcd);
        } else {
            numerator = num;
            denominator = den;
        }
    }

    /**
     * Private constructor: Instances are created using factory methods.
     *
     * <p>This sets the denominator to 1.
     *
     * @param num Numerator (must not be null).
     */
    private BigFraction(BigInteger num) {
        numerator = num;
        denominator = BigInteger.ONE;
    }

    /**
     * Create a fraction given the double value and either the maximum
     * error allowed or the maximum number of denominator digits.
     *
     * <p>
     * NOTE: This method is called with:
     * </p>
     * <ul>
     *  <li>EITHER a valid epsilon value and the maxDenominator set to
     *      Integer.MAX_VALUE (that way the maxDenominator has no effect)
     *  <li>OR a valid maxDenominator value and the epsilon value set to
     *      zero (that way epsilon only has effect if there is an exact
     *      match before the maxDenominator value is reached).
     * </ul>
     * <p>
     * It has been done this way so that the same code can be reused for
     * both scenarios. However this could be confusing to users if it
     * were part of the public API and this method should therefore remain
     * PRIVATE.
     * </p>
     *
     * <p>
     * See JIRA issue ticket MATH-181 for more details:
     *     https://issues.apache.org/jira/browse/MATH-181
     * </p>
     *
     * @param value Value to convert to a fraction.
     * @param epsilon Maximum error allowed.
     * The resulting fraction is within {@code epsilon} of {@code value},
     * in absolute terms.
     * @param maxDenominator Maximum denominator value allowed.
     * @param maxIterations Maximum number of convergents.
     * @return a new instance.
     * @throws IllegalArgumentException if the given {@code value} is NaN or infinite.
     * @throws ArithmeticException if the continued fraction failed to converge.
     */
    private static BigFraction from(final double value,
                                    final double epsilon,
                                    final int maxDenominator,
                                    final int maxIterations) {
        if (!Double.isFinite(value)) {
            throw new IllegalArgumentException(NOT_FINITE + value);
        }
        if (value == 0) {
            return ZERO;
        }

        // Remove sign, this is restored at the end.
        // (Assumes the value is not zero and thus signum(value) is not zero).
        final double absValue = Math.abs(value);
        double r0 = absValue;
        long a0 = (long) Math.floor(r0);
        if (a0 > OVERFLOW) {
            throw new FractionException(FractionException.ERROR_CONVERSION_OVERFLOW, value, a0, 1);
        }

        // check for (almost) integer arguments, which should not go to iterations.
        if (r0 - a0 <= epsilon) {
            // Restore the sign.
            if (value < 0) {
                a0 = -a0;
            }
            return new BigFraction(BigInteger.valueOf(a0));
        }

        // Support 2^31 as maximum denominator.
        // This is negative as an integer so convert to long.
        final long maxDen = Math.abs((long) maxDenominator);

        long p0 = 1;
        long q0 = 0;
        long p1 = a0;
        long q1 = 1;

        long p2;
        long q2;

        int n = 0;
        boolean stop = false;
        do {
            ++n;
            final double r1 = 1.0 / (r0 - a0);
            final long a1 = (long) Math.floor(r1);
            p2 = (a1 * p1) + p0;
            q2 = (a1 * q1) + q0;
            if (Long.compareUnsigned(p2, OVERFLOW) > 0 ||
                Long.compareUnsigned(q2, OVERFLOW) > 0) {
                // In maxDenominator mode, fall-back to the previous valid fraction.
                if (epsilon == 0) {
                    p2 = p1;
                    q2 = q1;
                    break;
                }
                throw new FractionException(FractionException.ERROR_CONVERSION_OVERFLOW, value, p2, q2);
            }

            final double convergent = (double) p2 / (double) q2;
            if (n < maxIterations &&
                Math.abs(convergent - absValue) > epsilon &&
                q2 < maxDen) {
                p0 = p1;
                p1 = p2;
                q0 = q1;
                q1 = q2;
                a0 = a1;
                r0 = r1;
            } else {
                stop = true;
            }
        } while (!stop);

        if (n >= maxIterations) {
            throw new FractionException(FractionException.ERROR_CONVERSION, value, maxIterations);
        }

        // Use p2 / q2 or p1 / q1 if q2 has grown too large in maxDenominator mode
        long num;
        long den;
        if (q2 <= maxDen) {
            num = p2;
            den = q2;
        } else {
            num = p1;
            den = q1;
        }

        // Restore the sign.
        if (Math.signum(num) * Math.signum(den) != Math.signum(value)) {
            num = -num;
        }

        return new BigFraction(BigInteger.valueOf(num),
                               BigInteger.valueOf(den));
    }

    /**
     * Create a fraction given the double value.
     * <p>
     * This factory method behaves <em>differently</em> to the method
     * {@link #from(double, double, int)}. It converts the double value
     * exactly, considering its internal bits representation. This works for all
     * values except NaN and infinities and does not requires any loop or
     * convergence threshold.
     * </p>
     * <p>
     * Since this conversion is exact and since double numbers are sometimes
     * approximated, the fraction created may seem strange in some cases. For example,
     * calling {@code from(1.0 / 3.0)} does <em>not</em> create
     * the fraction \( \frac{1}{3} \), but the fraction \( \frac{6004799503160661}{18014398509481984} \)
     * because the double number passed to the method is not exactly \( \frac{1}{3} \)
     * (which cannot be represented exactly in IEEE754).
     * </p>
     *
     * @param value Value to convert to a fraction.
     * @throws IllegalArgumentException if the given {@code value} is NaN or infinite.
     * @return a new instance.
     *
     * @see #from(double,double,int)
     */
    public static BigFraction from(final double value) {
        if (!Double.isFinite(value)) {
            throw new IllegalArgumentException(NOT_FINITE + value);
        }
        if (value == 0) {
            return ZERO;
        }

        final long bits = Double.doubleToLongBits(value);
        final long sign = bits & 0x8000000000000000L;
        final long exponent = bits & 0x7ff0000000000000L;
        final long mantissa = bits & 0x000fffffffffffffL;

        // Compute m and k such that value = m * 2^k
        long m;
        int k;

        if (exponent == 0) {
            // Subnormal number, the effective exponent bias is 1022, not 1023.
            // Note: mantissa is never zero as that case has been eliminated.
            m = mantissa;
            k = -1074;
        } else {
            // Normalized number: Add the implicit most significant bit.
            m = mantissa | 0x0010000000000000L;
            k = ((int) (exponent >> 52)) - 1075; // Exponent bias is 1023.
        }
        if (sign != 0) {
            m = -m;
        }
        while ((m & 0x001ffffffffffffeL) != 0 &&
               (m & 0x1) == 0) {
            m >>= 1;
            ++k;
        }

        return k < 0 ?
            new BigFraction(BigInteger.valueOf(m),
                            BigInteger.ZERO.flipBit(-k)) :
            new BigFraction(BigInteger.valueOf(m).multiply(BigInteger.ZERO.flipBit(k)),
                            BigInteger.ONE);
    }

    /**
     * Create a fraction given the double value and maximum error allowed.
     * <p>
     * References:
     * <ul>
     * <li><a href="http://mathworld.wolfram.com/ContinuedFraction.html">
     * Continued Fraction</a> equations (11) and (22)-(26)</li>
     * </ul>
     *
     * @param value Value to convert to a fraction.
     * @param epsilon Maximum error allowed. The resulting fraction is within
     * {@code epsilon} of {@code value}, in absolute terms.
     * @param maxIterations Maximum number of convergents.
     * @throws IllegalArgumentException if the given {@code value} is NaN or infinite;
     * {@code epsilon} is not positive; or {@code maxIterations < 1}.
     * @throws ArithmeticException if the continued fraction failed to converge.
     * @return a new instance.
     */
    public static BigFraction from(final double value,
                                   final double epsilon,
                                   final int maxIterations) {
        if (maxIterations < 1) {
            throw new IllegalArgumentException("Max iterations must be strictly positive: " + maxIterations);
        }
        if (epsilon >= 0) {
            return from(value, epsilon, Integer.MIN_VALUE, maxIterations);
        }
        throw new IllegalArgumentException("Epsilon must be positive: " + maxIterations);
    }

    /**
     * Create a fraction given the double value and maximum denominator.
     *
     * <p>
     * References:
     * <ul>
     * <li><a href="http://mathworld.wolfram.com/ContinuedFraction.html">
     * Continued Fraction</a> equations (11) and (22)-(26)</li>
     * </ul>
     *
     * <p>Note: The magnitude of the {@code maxDenominator} is used allowing use of
     * {@link Integer#MIN_VALUE} for a supported maximum denominator of 2<sup>31</sup>.
     *
     * @param value Value to convert to a fraction.
     * @param maxDenominator Maximum allowed value for denominator.
     * @throws IllegalArgumentException if the given {@code value} is NaN or infinite
     * or {@code maxDenominator} is zero.
     * @throws ArithmeticException if the continued fraction failed to converge.
     * @return a new instance.
     */
    public static BigFraction from(final double value,
                                   final int maxDenominator) {
        if (maxDenominator == 0) {
            // Re-use the zero denominator message
            throw new IllegalArgumentException(FractionException.ERROR_ZERO_DENOMINATOR);
        }
        return from(value, 0, maxDenominator, DEFAULT_MAX_ITERATIONS);
    }

    /**
     * Create a fraction given the numerator. The denominator is {@code 1}.
     *
     * @param num
     *            the numerator.
     * @return a new instance.
     */
    public static BigFraction of(final int num) {
        if (num == 0) {
            return ZERO;
        }
        return new BigFraction(BigInteger.valueOf(num));
    }

    /**
     * Create a fraction given the numerator. The denominator is {@code 1}.
     *
     * @param num Numerator.
     * @return a new instance.
     */
    public static BigFraction of(final long num) {
        if (num == 0) {
            return ZERO;
        }
        return new BigFraction(BigInteger.valueOf(num));
    }

    /**
     * Create a fraction given the numerator. The denominator is {@code 1}.
     *
     * @param num Numerator.
     * @return a new instance.
     * @throws NullPointerException if numerator is null.
     */
    public static BigFraction of(final BigInteger num) {
        Objects.requireNonNull(num, "numerator");
        if (num.signum() == 0) {
            return ZERO;
        }
        return new BigFraction(num);
    }

    /**
     * Create a fraction given the numerator and denominator.
     * The fraction is reduced to lowest terms.
     *
     * @param num Numerator.
     * @param den Denominator.
     * @return a new instance.
     * @throws ArithmeticException if {@code den} is zero.
     */
    public static BigFraction of(final int num, final int den) {
        if (num == 0) {
            return ZERO;
        }
        return new BigFraction(BigInteger.valueOf(num), BigInteger.valueOf(den));
    }

    /**
     * Create a fraction given the numerator and denominator.
     * The fraction is reduced to lowest terms.
     *
     * @param num Numerator.
     * @param den Denominator.
     * @return a new instance.
     * @throws ArithmeticException if {@code den} is zero.
     */
    public static BigFraction of(final long num, final long den) {
        if (num == 0) {
            return ZERO;
        }
        return new BigFraction(BigInteger.valueOf(num), BigInteger.valueOf(den));
    }

    /**
     * Create a fraction given the numerator and denominator.
     * The fraction is reduced to lowest terms.
     *
     * @param num Numerator.
     * @param den Denominator.
     * @return a new instance.
     * @throws NullPointerException if numerator or denominator are null.
     * @throws ArithmeticException if the denominator is zero.
     */
    public static BigFraction of(final BigInteger num, final BigInteger den) {
        if (num.signum() == 0) {
            return ZERO;
        }
        return new BigFraction(num, den);
    }

    /**
     * Returns a {@code BigFraction} instance representing the specified string {@code s}.
     *
     * <p>If {@code s} is {@code null}, then a {@code NullPointerException} is thrown.
     *
     * <p>The string must be in a format compatible with that produced by
     * {@link #toString() BigFraction.toString()}.
     * The format expects an integer optionally followed by a {@code '/'} character and
     * and second integer. Leading and trailing spaces are allowed around each numeric part.
     * Each numeric part is parsed using {@link BigInteger#BigInteger(String)}. The parts
     * are interpreted as the numerator and optional denominator of the fraction. If absent
     * the denominator is assumed to be "1".
     *
     * <p>Examples of valid strings and the equivalent {@code BigFraction} are shown below:
     *
     * <pre>
     * "0"                 = BigFraction.of(0)
     * "42"                = BigFraction.of(42)
     * "0 / 1"             = BigFraction.of(0, 1)
     * "1 / 3"             = BigFraction.of(1, 3)
     * "-4 / 13"           = BigFraction.of(-4, 13)</pre>
     *
     * <p>Note: The fraction is returned in reduced form and the numerator and denominator
     * may not match the values in the input string. For this reason the result of
     * {@code BigFraction.parse(s).toString().equals(s)} may not be {@code true}.
     *
     * @param s String representation.
     * @return an instance.
     * @throws NullPointerException if the string is null.
     * @throws NumberFormatException if the string does not contain a parsable fraction.
     * @see BigInteger#BigInteger(String)
     * @see #toString()
     */
    public static BigFraction parse(String s) {
        final String stripped = s.replace(",", "");
        final int slashLoc = stripped.indexOf('/');
        // if no slash, parse as single number
        if (slashLoc == -1) {
            return of(new BigInteger(stripped.trim()));
        }
        final BigInteger num = new BigInteger(stripped.substring(0, slashLoc).trim());
        final BigInteger denom = new BigInteger(stripped.substring(slashLoc + 1).trim());
        return of(num, denom);
    }

    @Override
    public BigFraction zero() {
        return ZERO;
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.2
     */
    @Override
    public boolean isZero() {
        return numerator.signum() == 0;
    }

    @Override
    public BigFraction one() {
        return ONE;
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.2
     */
    @Override
    public boolean isOne() {
        return numerator.equals(denominator);
    }

    /**
     * Access the numerator as a {@code BigInteger}.
     *
     * @return the numerator as a {@code BigInteger}.
     */
    public BigInteger getNumerator() {
        return numerator;
    }

    /**
     * Access the numerator as an {@code int}.
     *
     * @return the numerator as an {@code int}.
     */
    public int getNumeratorAsInt() {
        return numerator.intValue();
    }

    /**
     * Access the numerator as a {@code long}.
     *
     * @return the numerator as a {@code long}.
     */
    public long getNumeratorAsLong() {
        return numerator.longValue();
    }

    /**
     * Access the denominator as a {@code BigInteger}.
     *
     * @return the denominator as a {@code BigInteger}.
     */
    public BigInteger getDenominator() {
        return denominator;
    }

    /**
     * Access the denominator as an {@code int}.
     *
     * @return the denominator as an {@code int}.
     */
    public int getDenominatorAsInt() {
        return denominator.intValue();
    }

    /**
     * Access the denominator as a {@code long}.
     *
     * @return the denominator as a {@code long}.
     */
    public long getDenominatorAsLong() {
        return denominator.longValue();
    }

    /**
     * Retrieves the sign of this fraction.
     *
     * @return -1 if the value is strictly negative, 1 if it is strictly
     * positive, 0 if it is 0.
     */
    public int signum() {
        return numerator.signum() * denominator.signum();
    }

    /**
     * Returns the absolute value of this fraction.
     *
     * @return the absolute value.
     */
    public BigFraction abs() {
        return signum() >= 0 ?
            this :
            negate();
    }

    @Override
    public BigFraction negate() {
        return new BigFraction(numerator.negate(), denominator);
    }

    /**
     * {@inheritDoc}
     *
     * <p>Raises an exception if the fraction is equal to zero.
     *
     * @throws ArithmeticException if the current numerator is {@code zero}
     */
    @Override
    public BigFraction reciprocal() {
        return new BigFraction(denominator, numerator);
    }

    /**
     * Returns the {@code double} value closest to this fraction.
     *
     * @return the fraction as a {@code double}.
     */
    @Override
    public double doubleValue() {
        return Double.longBitsToDouble(toFloatingPointBits(11, 52));
    }

    /**
     * Returns the {@code float} value closest to this fraction.
     *
     * @return the fraction as a {@code double}.
     */
    @Override
    public float floatValue() {
        return Float.intBitsToFloat((int) toFloatingPointBits(8, 23));
    }

    /**
     * Returns the whole number part of the fraction.
     *
     * @return the largest {@code int} value that is not larger than this fraction.
     */
    @Override
    public int intValue() {
        return numerator.divide(denominator).intValue();
    }

    /**
     * Returns the whole number part of the fraction.
     *
     * @return the largest {@code long} value that is not larger than this fraction.
     */
    @Override
    public long longValue() {
        return numerator.divide(denominator).longValue();
    }

    /**
     * Returns the {@code BigDecimal} representation of this fraction.
     * This calculates the fraction as numerator divided by denominator.
     *
     * @return the fraction as a {@code BigDecimal}.
     * @throws ArithmeticException
     *             if the exact quotient does not have a terminating decimal
     *             expansion.
     * @see BigDecimal
     */
    public BigDecimal bigDecimalValue() {
        return new BigDecimal(numerator).divide(new BigDecimal(denominator));
    }

    /**
     * Returns the {@code BigDecimal} representation of this fraction.
     * This calculates the fraction as numerator divided by denominator
     * following the passed rounding mode.
     *
     * @param roundingMode Rounding mode to apply.
     * @return the fraction as a {@code BigDecimal}.
     * @see BigDecimal
     */
    public BigDecimal bigDecimalValue(RoundingMode roundingMode) {
        return new BigDecimal(numerator).divide(new BigDecimal(denominator), roundingMode);
    }

    /**
     * Returns the {@code BigDecimal} representation of this fraction.
     * This calculates the fraction as numerator divided by denominator
     * following the passed scale and rounding mode.
     *
     * @param scale
     *            scale of the {@code BigDecimal} quotient to be returned.
     *            see {@link BigDecimal} for more information.
     * @param roundingMode Rounding mode to apply.
     * @return the fraction as a {@code BigDecimal}.
     * @throws ArithmeticException if {@code roundingMode} == {@link RoundingMode#UNNECESSARY} and
     *      the specified scale is insufficient to represent the result of the division exactly.
     * @see BigDecimal
     */
    public BigDecimal bigDecimalValue(final int scale, RoundingMode roundingMode) {
        return new BigDecimal(numerator).divide(new BigDecimal(denominator), scale, roundingMode);
    }

    /**
     * Adds the specified {@code value} to this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to add.
     * @return {@code this + value}.
     */
    public BigFraction add(final int value) {
        return add(BigInteger.valueOf(value));
    }

    /**
     * Adds the specified {@code value} to this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to add.
     * @return {@code this + value}.
     */
    public BigFraction add(final long value) {
        return add(BigInteger.valueOf(value));
    }

    /**
     * Adds the specified {@code value} to this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to add.
     * @return {@code this + value}.
     */
    public BigFraction add(final BigInteger value) {
        if (value.signum() == 0) {
            return this;
        }
        if (isZero()) {
            return of(value);
        }

        return of(numerator.add(denominator.multiply(value)), denominator);
    }

    /**
     * Adds the specified {@code value} to this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to add.
     * @return {@code this + value}.
     */
    @Override
    public BigFraction add(final BigFraction value) {
        if (value.isZero()) {
            return this;
        }
        if (isZero()) {
            return value;
        }

        final BigInteger num;
        final BigInteger den;

        if (denominator.equals(value.denominator)) {
            num = numerator.add(value.numerator);
            den = denominator;
        } else {
            num = (numerator.multiply(value.denominator)).add((value.numerator).multiply(denominator));
            den = denominator.multiply(value.denominator);
        }

        if (num.signum() == 0) {
            return ZERO;
        }

        return new BigFraction(num, den);
    }

    /**
     * Subtracts the specified {@code value} from this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to subtract.
     * @return {@code this - value}.
     */
    public BigFraction subtract(final int value) {
        return subtract(BigInteger.valueOf(value));
    }

    /**
     * Subtracts the specified {@code value} from this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to subtract.
     * @return {@code this - value}.
     */
    public BigFraction subtract(final long value) {
        return subtract(BigInteger.valueOf(value));
    }

    /**
     * Subtracts the specified {@code value} from this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to subtract.
     * @return {@code this - value}.
     */
    public BigFraction subtract(final BigInteger value) {
        if (value.signum() == 0) {
            return this;
        }
        if (isZero()) {
            return of(value.negate());
        }

        return of(numerator.subtract(denominator.multiply(value)), denominator);
    }

    /**
     * Subtracts the specified {@code value} from this fraction, returning
     * the result in reduced form.
     *
     * @param value Value to subtract.
     * @return {@code this - value}.
     */
    @Override
    public BigFraction subtract(final BigFraction value) {
        if (value.isZero()) {
            return this;
        }
        if (isZero()) {
            return value.negate();
        }

        final BigInteger num;
        final BigInteger den;
        if (denominator.equals(value.denominator)) {
            num = numerator.subtract(value.numerator);
            den = denominator;
        } else {
            num = (numerator.multiply(value.denominator)).subtract((value.numerator).multiply(denominator));
            den = denominator.multiply(value.denominator);
        }

        if (num.signum() == 0) {
            return ZERO;
        }

        return new BigFraction(num, den);
    }

    /**
     * Multiply this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to multiply by.
     * @return {@code this * value}.
     */
    @Override
    public BigFraction multiply(final int value) {
        if (value == 0 || isZero()) {
            return ZERO;
        }

        return multiply(BigInteger.valueOf(value));
    }

    /**
     * Multiply this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to multiply by.
     * @return {@code this * value}.
     */
    public BigFraction multiply(final long value) {
        if (value == 0 || isZero()) {
            return ZERO;
        }

        return multiply(BigInteger.valueOf(value));
    }

    /**
     * Multiply this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to multiply by.
     * @return {@code this * value}.
     */
    public BigFraction multiply(final BigInteger value) {
        if (value.signum() == 0 || isZero()) {
            return ZERO;
        }
        return new BigFraction(value.multiply(numerator), denominator);
    }

    /**
     * Multiply this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to multiply by.
     * @return {@code this * value}.
     */
    @Override
    public BigFraction multiply(final BigFraction value) {
        if (value.isZero() || isZero()) {
            return ZERO;
        }
        return new BigFraction(numerator.multiply(value.numerator),
                               denominator.multiply(value.denominator));
    }

    /**
     * Divide this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to divide by
     * @return {@code this / value}.
     * @throws ArithmeticException if the value to divide by is zero
     */
    public BigFraction divide(final int value) {
        return divide(BigInteger.valueOf(value));
    }

    /**
     * Divide this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to divide by
     * @return {@code this / value}.
     * @throws ArithmeticException if the value to divide by is zero
     */
    public BigFraction divide(final long value) {
        return divide(BigInteger.valueOf(value));
    }

    /**
     * Divide this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to divide by
     * @return {@code this / value}.
     * @throws ArithmeticException if the value to divide by is zero
     */
    public BigFraction divide(final BigInteger value) {
        if (value.signum() == 0) {
            throw new FractionException(FractionException.ERROR_DIVIDE_BY_ZERO);
        }
        if (isZero()) {
            return ZERO;
        }
        return new BigFraction(numerator, denominator.multiply(value));
    }

    /**
     * Divide this fraction by the passed {@code value}, returning
     * the result in reduced form.
     *
     * @param value Value to divide by
     * @return {@code this / value}.
     * @throws ArithmeticException if the value to divide by is zero
     */
    @Override
    public BigFraction divide(final BigFraction value) {
        if (value.isZero()) {
            throw new FractionException(FractionException.ERROR_DIVIDE_BY_ZERO);
        }
        if (isZero()) {
            return ZERO;
        }
        // Multiply by reciprocal
        return new BigFraction(numerator.multiply(value.denominator),
                               denominator.multiply(value.numerator));
    }

    /**
     * Returns a {@code BigFraction} whose value is
     * <code>this<sup>exponent</sup></code>, returning the result in reduced form.
     *
     * @param exponent exponent to which this {@code BigFraction} is to be raised.
     * @return <code>this<sup>exponent</sup></code>.
     * @throws ArithmeticException if the intermediate result would overflow.
     */
    @Override
    public BigFraction pow(final int exponent) {
        if (exponent == 1) {
            return this;
        }
        if (exponent == 0) {
            return ONE;
        }
        if (isZero()) {
            if (exponent < 0) {
                throw new FractionException(FractionException.ERROR_ZERO_DENOMINATOR);
            }
            return ZERO;
        }
        if (exponent > 0) {
            return new BigFraction(numerator.pow(exponent),
                                   denominator.pow(exponent));
        }
        if (exponent == -1) {
            return this.reciprocal();
        }
        if (exponent == Integer.MIN_VALUE) {
            // MIN_VALUE can't be negated
            return new BigFraction(denominator.pow(Integer.MAX_VALUE).multiply(denominator),
                                   numerator.pow(Integer.MAX_VALUE).multiply(numerator));
        }
        // Note: Raise the BigIntegers to the power and then reduce.
        // The supported range for BigInteger is currently
        // +/-2^(Integer.MAX_VALUE) exclusive thus larger
        // exponents (long, BigInteger) are currently not supported.
        return new BigFraction(denominator.pow(-exponent),
                               numerator.pow(-exponent));
    }

    /**
     * Returns the {@code String} representing this fraction.
     * Uses:
     * <ul>
     *  <li>{@code "0"} if {@code numerator} is zero.
     *  <li>{@code "numerator"} if {@code denominator} is one.
     *  <li>{@code "numerator / denominator"} for all other cases.
     * </ul>
     *
     * @return a string representation of the fraction.
     */
    @Override
    public String toString() {
        final String str;
        if (isZero()) {
            str = "0";
        } else if (BigInteger.ONE.equals(denominator)) {
            str = numerator.toString();
        } else {
            str = numerator + " / " + denominator;
        }
        return str;
    }

    /**
     * Compares this object with the specified object for order using the signed magnitude.
     *
     * @param other {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public int compareTo(final BigFraction other) {
        final int lhsSigNum = signum();
        final int rhsSigNum = other.signum();

        if (lhsSigNum != rhsSigNum) {
            return (lhsSigNum > rhsSigNum) ? 1 : -1;
        }
        // Same sign.
        // Avoid a multiply if both fractions are zero
        if (lhsSigNum == 0) {
            return 0;
        }
        // Compare absolute magnitude
        final BigInteger nOd = numerator.abs().multiply(other.denominator.abs());
        final BigInteger dOn = denominator.abs().multiply(other.numerator.abs());
        return lhsSigNum > 0 ?
            nOd.compareTo(dOn) :
            dOn.compareTo(nOd);
    }

    /**
     * Test for equality with another object. If the other object is a {@code Fraction} then a
     * comparison is made of the sign and magnitude; otherwise {@code false} is returned.
     *
     * @param other {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }

        if (other instanceof BigFraction) {
            // Since fractions are always in lowest terms, numerators and
            // denominators can be compared directly for equality.
            final BigFraction rhs = (BigFraction) other;
            if (signum() == rhs.signum()) {
                return numerator.abs().equals(rhs.numerator.abs()) &&
                       denominator.abs().equals(rhs.denominator.abs());
            }
        }

        return false;
    }

    @Override
    public int hashCode() {
        // Incorporate the sign and absolute values of the numerator and denominator.
        // Equivalent to:
        // int hash = 1;
        // hash = 31 * hash + numerator.abs().hashCode();
        // hash = 31 * hash + denominator.abs().hashCode();
        // hash = hash * signum()
        // Note: BigInteger.hashCode() * BigInteger.signum() == BigInteger.abs().hashCode().
        final int numS = numerator.signum();
        final int denS = denominator.signum();
        return (31 * (31 + numerator.hashCode() * numS) + denominator.hashCode() * denS) * numS * denS;
    }

    /**
     * Calculates the sign bit, the biased exponent and the significand for a
     * binary floating-point representation of this {@code BigFraction}
     * according to the IEEE 754 standard, and encodes these values into a {@code long}
     * variable. The representative bits are arranged adjacent to each other and
     * placed at the low-order end of the returned {@code long} value, with the
     * least significant bits used for the significand, the next more
     * significant bits for the exponent, and next more significant bit for the
     * sign.
     *
     * <p>Warning: The arguments are not validated.
     *
     * @param exponentLength the number of bits allowed for the exponent; must be
     *                       between 1 and 32 (inclusive), and must not be greater
     *                       than {@code 63 - significandLength}
     * @param significandLength the number of bits allowed for the significand
     *                          (excluding the implicit leading 1-bit in
     *                          normalized numbers, e.g. 52 for a double-precision
     *                          floating-point number); must be between 1 and
     *                          {@code 63 - exponentLength} (inclusive)
     * @return the bits of an IEEE 754 binary floating-point representation of
     * this fraction encoded in a {@code long}, as described above.
     */
    private long toFloatingPointBits(int exponentLength, int significandLength) {
        // Assume the following conditions:
        //assert exponentLength >= 1;
        //assert exponentLength <= 32;
        //assert significandLength >= 1;
        //assert significandLength <= 63 - exponentLength;

        if (isZero()) {
            return 0L;
        }

        final long sign = (numerator.signum() * denominator.signum()) == -1 ? 1L : 0L;
        final BigInteger positiveNumerator = numerator.abs();
        final BigInteger positiveDenominator = denominator.abs();

        /*
         * The most significant 1-bit of a non-zero number is not explicitly
         * stored in the significand of an IEEE 754 normalized binary
         * floating-point number, so we need to round the value of this fraction
         * to (significandLength + 1) bits. In order to do this, we calculate
         * the most significant (significandLength + 2) bits, and then, based on
         * the least significant of those bits, find out whether we need to
         * round up or down.
         *
         * First, we'll remove all powers of 2 from the denominator because they
         * are not relevant for the significand of the prospective binary
         * floating-point value.
         */
        final int denRightShift = positiveDenominator.getLowestSetBit();
        final BigInteger divisor = positiveDenominator.shiftRight(denRightShift);

        /*
         * Now, we're going to calculate the (significandLength + 2) most
         * significant bits of the fraction's value using integer division. To
         * guarantee that the quotient of the division has at least
         * (significandLength + 2) bits, the bit length of the dividend must
         * exceed that of the divisor by at least that amount.
         *
         * If the denominator has prime factors other than 2, i.e. if the
         * divisor was not reduced to 1, an excess of exactly
         * (significandLength + 2) bits is sufficient, because the knowledge
         * that the fractional part of the precise quotient's binary
         * representation does not terminate is enough information to resolve
         * cases where the most significant (significandLength + 2) bits alone
         * are not conclusive.
         *
         * Otherwise, the quotient must be calculated exactly and the bit length
         * of the numerator can only be reduced as long as no precision is lost
         * in the process (meaning it can have powers of 2 removed, like the
         * denominator).
         */
        int numRightShift = positiveNumerator.bitLength() - divisor.bitLength() - (significandLength + 2);
        if (numRightShift > 0 &&
            divisor.equals(BigInteger.ONE)) {
            numRightShift = Math.min(numRightShift, positiveNumerator.getLowestSetBit());
        }
        final BigInteger dividend = positiveNumerator.shiftRight(numRightShift);

        final BigInteger quotient = dividend.divide(divisor);

        int quotRightShift = quotient.bitLength() - (significandLength + 1);
        long significand = roundAndRightShift(
                quotient,
                quotRightShift,
                !divisor.equals(BigInteger.ONE)
        ).longValue();

        /*
         * If the significand had to be rounded up, this could have caused the
         * bit length of the significand to increase by one.
         */
        if ((significand & (1L << (significandLength + 1))) != 0) {
            significand >>= 1;
            quotRightShift++;
        }

        /*
         * Now comes the exponent. The absolute value of this fraction based on
         * the current local variables is:
         *
         * significand * 2^(numRightShift - denRightShift + quotRightShift)
         *
         * To get the unbiased exponent for the floating-point value, we need to
         * add (significandLength) to the above exponent, because all but the
         * most significant bit of the significand will be treated as a
         * fractional part. To convert the unbiased exponent to a biased
         * exponent, we also need to add the exponent bias.
         */
        final int exponentBias = (1 << (exponentLength - 1)) - 1;
        long exponent = (long) numRightShift - denRightShift + quotRightShift + significandLength + exponentBias;
        final long maxExponent = (1L << exponentLength) - 1L; //special exponent for infinities and NaN

        if (exponent >= maxExponent) { //infinity
            exponent = maxExponent;
            significand = 0L;
        } else if (exponent > 0) { //normalized number
            significand &= -1L >>> (64 - significandLength); //remove implicit leading 1-bit
        } else { //smaller than the smallest normalized number
            /*
             * We need to round the quotient to fewer than
             * (significandLength + 1) bits. This must be done with the original
             * quotient and not with the current significand, because the loss
             * of precision in the previous rounding might cause a rounding of
             * the current significand's value to produce a different result
             * than a rounding of the original quotient.
             *
             * So we find out how many high-order bits from the quotient we can
             * transfer into the significand. The absolute value of the fraction
             * is:
             *
             * quotient * 2^(numRightShift - denRightShift)
             *
             * To get the significand, we need to right shift the quotient so
             * that the above exponent becomes (1 - exponentBias - significandLength)
             * (the unbiased exponent of a subnormal floating-point number is
             * defined as equivalent to the minimum unbiased exponent of a
             * normalized floating-point number, and (- significandLength)
             * because the significand will be treated as the fractional part).
             */
            significand = roundAndRightShift(quotient,
                                             (1 - exponentBias - significandLength) - (numRightShift - denRightShift),
                                             !divisor.equals(BigInteger.ONE)).longValue();
            exponent = 0L;

            /*
             * Note: It is possible that an otherwise subnormal number will
             * round up to the smallest normal number. However, this special
             * case does not need to be treated separately, because the
             * overflowing highest-order bit of the significand will then simply
             * become the lowest-order bit of the exponent, increasing the
             * exponent from 0 to 1 and thus establishing the implicity of the
             * leading 1-bit.
             */
        }

        return (sign << (significandLength + exponentLength)) |
            (exponent << significandLength) |
            significand;
    }

    /**
     * Rounds an integer to the specified power of two (i.e. the minimum number of
     * low-order bits that must be zero) and performs a right-shift by this
     * amount. The rounding mode applied is round to nearest, with ties rounding
     * to even (meaning the prospective least significant bit must be zero). The
     * number can optionally be treated as though it contained at
     * least one 0-bit and one 1-bit in its fractional part, to influence the result in cases
     * that would otherwise be a tie.
     * @param value the number to round and right-shift
     * @param bits the power of two to which to round; must be positive
     * @param hasFractionalBits whether the number should be treated as though
     *                          it contained a non-zero fractional part
     * @return a {@code BigInteger} as described above
     */
    private static BigInteger roundAndRightShift(BigInteger value, int bits, boolean hasFractionalBits) {
        // Assumptions:
        // assert bits > 0

        BigInteger result = value.shiftRight(bits);
        if (value.testBit(bits - 1) &&
            (hasFractionalBits ||
             value.getLowestSetBit() < bits - 1 ||
             value.testBit(bits))) {
            result = result.add(BigInteger.ONE); //round up
        }

        return result;
    }
}

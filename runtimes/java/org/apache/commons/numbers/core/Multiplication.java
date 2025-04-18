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
package org.apache.commons.numbers.core;

/**
 * Multiplication.
 *
 * @param <T> Type of elements.
 */
public interface Multiplication<T> {
    /**
     * Binary multiplication.
     *
     * @param a Element.
     * @return {@code this * a}.
     */
    T multiply(T a);

    /**
     * Identity element.
     *
     * @return the field element such that for all {@code a},
     * {@code one().multiply(a).equals(a)} is {@code true}.
     */
    T one();

    /**
     * Multiplicative inverse.
     *
     * @return <code>this<sup>-1</sup></code>.
     */
    T reciprocal();

    /**
     * Check if this is a neutral element of multiplication, i.e. {@code this.multiply(a)} returns
     * {@code a} or an element representing the same value as {@code a}.
     *
     * <p>The default implementation calls {@link Object#equals(Object) equals(one())}.
     * Implementations may want to employ more a efficient method. This may even
     * be required if an implementation has multiple representations of {@code one} and its
     * {@code equals} method differentiates between them.
     *
     * @return {@code true} if {@code this} is a neutral element of multiplication.
     * @see #one()
     * @since 1.2
     */
    default boolean isOne() {
        return this.equals(one());
    }
}

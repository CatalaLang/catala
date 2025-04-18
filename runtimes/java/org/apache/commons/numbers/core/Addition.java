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
 * Addition.
 *
 * @param <T> Type of elements.
 */
public interface Addition<T> {
    /**
     * Binary addition.
     *
     * @param a Element.
     * @return {@code this + a}.
     */
    T add(T a);

    /**
     * Identity element.
     *
     * @return the field element such that for all {@code a},
     * {@code zero().add(a).equals(a)} is {@code true}.
     */
    T zero();

    /**
     * Additive inverse.
     *
     * @return {@code -this}.
     */
    T negate();

    /**
     * Check if this is a neutral element of addition, i.e. {@code this.add(a)} returns
     * {@code a} or an element representing the same value as {@code a}.
     *
     * <p>The default implementation calls {@link Object#equals(Object) equals(zero())}.
     * Implementations may want to employ more a efficient method. This may even
     * be required if an implementation has multiple representations of {@code zero} and its
     * {@code equals} method differentiates between them.
     *
     * @return {@code true} if {@code this} is a neutral element of addition.
     * @see #zero()
     * @since 1.2
     */
    default boolean isZero() {
        return this.equals(zero());
    }
}

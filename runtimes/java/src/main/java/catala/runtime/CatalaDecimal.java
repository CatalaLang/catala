package catala.runtime;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import org.apache.commons.numbers.fraction.BigFraction;

// We probably want to keep this class as-is
// and not derive it from BigFraction as BigFraction is not part of 
// java's stdlib and is currently vendored. (note: should we shadow
// the package name for BigFraction?)
public final class CatalaDecimal implements CatalaValue, Comparable<CatalaDecimal> {
  private final BigFraction value;

  private /* not sure of this? We might want to avoid let BigFraction escape in the API? */
  CatalaDecimal(BigFraction value){
    this.value = value;
  }

  //keeping this package-level for now
  CatalaDecimal(int num, int den){
    this.value = BigFraction.of(num, den);
  }

  public CatalaDecimal negate(){
    return new CatalaDecimal(this.value.negate());
  }

  // XXX probably not the display format that we want
  @Override
  public String toString(){
    return this.value.toString();
  }

  /**
   * @param other {@inheritDoc}
   * @return {@inheritDoc}
   */
  @Override
  public int compareTo(CatalaDecimal other){
    return this.value.compareTo(other.value);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    CatalaDecimal other = (CatalaDecimal) obj;
    return this.value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return this.value.hashCode();
  }

  public final BigInteger getDenominator(){
    return this.value.getDenominator();
  }

  public final BigInteger getNumerator(){
    return this.value.getNumerator();
  }

  public final CatalaDecimal multiply(CatalaDecimal other){
    return new CatalaDecimal(this.value.multiply(other.value));
  }

  final BigDecimal bigDecimalValue(int scale, RoundingMode roundingMode){
    return this.value.bigDecimalValue(scale, roundingMode);
  }

  // ToRat_int
  public static final CatalaDecimal ofInteger(CatalaInteger ci){
    return new CatalaDecimal(BigFraction.of(ci.asBigInteger()));
  }

  // ToRat_mon
  public static final CatalaDecimal ofMoney(CatalaMoney cm){
    return new CatalaDecimal(BigFraction.of(cm.asCents(), BigInteger.valueOf(100)));
  }

}
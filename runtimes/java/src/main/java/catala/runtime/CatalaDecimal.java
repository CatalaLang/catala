package catala.runtime;

import java.math.BigInteger;

import org.apache.commons.numbers.fraction.BigFraction;

// We probably want to keep this class as-is
// and not derive it from BigFraction as BigFraction is not part of 
// java's stdlib and is currently vendored. (note: should we shadow
// the package name for BigFraction?)
public final class CatalaDecimal implements CatalaValue {
  private final BigFraction value;

  private /* not sure of this? We might want to avoid let BigFraction escape in the API? */
  CatalaDecimal(BigFraction value){
    this.value = value;
  }

  @Override
  public String toString(){
    return this.value.toString();
  }

  public final BigInteger getDenominator(){
    return this.value.getDenominator();
  }

  public final BigInteger getNumerator(){
    return this.value.getNumerator();
  }

  // ToRat_int
  public static final CatalaDecimal ofInteger(CatalaInteger ci){
    return new CatalaDecimal(BigFraction.of(ci.asBigInteger()));
  }

}
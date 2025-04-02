package catala.runtime;

public sealed interface CatalaValue permits
  CatalaDecimal,
  CatalaInteger,
  CatalaMoney,
  CatalaUnit,
  CatalaBool,
  CatalaOption,
  CatalaTuple {}

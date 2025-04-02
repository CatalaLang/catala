package catala.runtime;

public sealed interface CatalaValue permits
  CatalaDate,
  CatalaDecimal,
  CatalaInteger,
  CatalaMoney,
  CatalaUnit,
  CatalaBool,
  CatalaOption,
  CatalaTuple {}

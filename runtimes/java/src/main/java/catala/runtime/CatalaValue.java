package catala.runtime;

public sealed interface CatalaValue permits
  CatalaDate,
  CatalaDuration,
  CatalaDecimal,
  CatalaInteger,
  CatalaMoney,
  CatalaUnit,
  CatalaBool,
  CatalaOption,
  CatalaTuple {}

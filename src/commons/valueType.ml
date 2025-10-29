type t = TInteger | TRational | TMoney | TEvent | TDate | TDuration

let encoding =
  Json_encoding.string_enum
    [
      ("int", TInteger);
      ("rational", TRational);
      ("money", TMoney);
      ("event", TEvent);
      ("date", TDate);
      ("duration", TDuration);
    ]

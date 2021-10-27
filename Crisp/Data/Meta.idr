module Crisp.Data.Meta

public export
record SourceFile where
  path : Maybe String
  body : String

public export
record Span where
  source : SourceFile
  start  : Nat
  end    : Nat

public export
record PrefixSpan where
  pre  : Span
  post : Span

public export
record SurroundSpan where
  start : Span
  end   : Span

public export
record NilMeta where
  span : Span

public export
record IntMeta where
  span : Span

public export
record StringMeta where
  span : SurroundSpan

public export
record SymbolMeta where
  span : SurroundSpan

public export
record QuoteMeta where
  span : PrefixSpan

public export
record ListMeta where
  span : SurroundSpan

public export
record VecMeta where
  span : SurroundSpan

DiagnosticSeverity <- list(
  Error = 1,
  Warning = 2,
  Information = 3,
  Hint = 4
)

FileChangeType <- list(
  Created = 1,
  Changed = 2,
  Deleted = 3
)

# as defined by the language server protocol
SymbolKind <- list(
  File = 1,
  Module = 2,
  Namespace = 3,
  Package = 4,
  Class = 5,
  Method = 6,
  Property = 7,
  Field = 8,
  Constructor = 9,
  Enum = 10,
  Interface = 11,
  Function = 12,
  Variable = 13,
  Constant = 14,
  String = 15,
  Number = 16,
  Boolean = 17,
  Array = 18,
  Object = 19,
  Key = 20,
  Null = 21,
  EnumMember = 22,
  Struct = 23,
  Event = 24,
  Operator = 25,
  TypeParameter = 26
)

SaveOptions <- list(
  includeText = TRUE
)

TextDocumentSyncKind <- list(
  None = 0,
  Full = 1,
  Incremental = 2
)

CodeActionKind <- list(
  Empty = "",
  QuickFix = "quickfix",
  Refactor = "refactor",
  RefactorExtract = "refactor.extract",
  RefactorInline = "refactor.inline",
  RefactorRewrite = "refactor.rewrite",
  Source = "source",
  SourceOrganizeImports = "source.organizeImports"
)

CompletionItemKind <- list(
  Text = 1,
  Method = 2,
  Function = 3,
  Constructor = 4,
  Field = 5,
  Variable = 6,
  Class = 7,
  Interface = 8,
  Module = 9,
  Property = 10,
  Unit = 11,
  Value = 12,
  Enum = 13,
  Keyword = 14,
  Snippet = 15,
  Color = 16,
  File = 17,
  Reference = 18,
  Folder = 19,
  EnumMember = 20,
  Constant = 21,
  Struct = 22,
  Event = 23,
  Operator = 24,
  TypeParameter = 25
)

InsertTextFormat <- list(
  PlainText = 1,
  Snippet = 2
)

DiagnosticSeverity <- list(
  Error = 1,
  Warning = 2,
  Information = 3,
  Hint = 4
)

FoldingRangeKind <- list(
  Comment = "comment",
  Imports = "imports",
  Region = "region"
)

DocumentHighlightKind <- list(
  Text = 1,
  Read = 2,
  Write = 3
)

TextDocumentSaveReason <- list(
  Manual = 1,
  AfterDelay = 2,
  FocusOut = 3
)

# Token types for R code
SemanticTokenTypes <- list(
  namespace = 0L,
  type = 1L,
  class = 2L,
  enum = 3L,
  interface = 4L,
  struct = 5L,
  typeParameter = 6L,
  parameter = 7L,
  variable = 8L,
  property = 9L,
  enumMember = 10L,
  event = 11L,
  `function` = 12L,
  method = 13L,
  macro = 14L,
  keyword = 15L,
  modifier = 16L,
  comment = 17L,
  string = 18L,
  number = 19L,
  regexp = 20L,
  operator = 21L,
  decorator = 22L
)

# Token modifiers
SemanticTokenModifiers <- list(
  declaration = 0L,
  definition = 1L,
  readonly = 2L,
  static = 3L,
  deprecated = 4L,
  abstract = 5L,
  async = 6L,
  modification = 7L,
  documentation = 8L,
  defaultLibrary = 9L
)

package runtime;

public record SourcePosition(
    String filename,
    int startLine,
    int startColumn,
    int endLine,
    int endColumn
){}
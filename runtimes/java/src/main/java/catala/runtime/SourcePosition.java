package catala.runtime;

public record SourcePosition(
        String filename,
        int startLine,
        int startColumn,
        int endLine,
        int endColumn,
        String[] law_headings) implements CatalaValue {}

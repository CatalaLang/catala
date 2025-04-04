package catala.runtime;

public record SourcePosition(
        String filename,
        int startLine,
        int startColumn,
        int endLine,
        int endColumn,
        String[] law_headings) implements CatalaValue {

    @Override
    public String toString() {
        return "in file " + filename + ", from " + startLine + ":" + startColumn + " to " + endLine + ":" + endColumn;
    }
}

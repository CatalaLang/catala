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

    @Override
    public CatalaBool equalsTo(CatalaValue other) {
        if (other instanceof SourcePosition o) {
            return CatalaBool.fromBoolean(this.filename.equals(o.filename) && this.startLine == o.startLine
                    && this.startColumn == o.startColumn && this.endLine == o.endLine);
        } else {
            return CatalaBool.FALSE;
        }
    }
}

package catala.runtime;

public record CatalaPosition(
        String filename,
        int startLine,
        int startColumn,
        int endLine,
        int endColumn,
        String[] law_headings) implements CatalaValue {

    @Override
    public String toString() {
        return filename + ":" + startLine + "." + startColumn + "-" + endLine + "." + endColumn;
    }

    @Override
    public CatalaBool equalsTo(CatalaValue other) {
        if (other instanceof CatalaPosition o) {
            return CatalaBool.fromBoolean(this.filename.equals(o.filename) && this.startLine == o.startLine
                    && this.startColumn == o.startColumn && this.endLine == o.endLine);
        } else {
            return CatalaBool.FALSE;
        }
    }
}

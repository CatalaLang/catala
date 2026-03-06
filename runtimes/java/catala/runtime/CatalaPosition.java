package catala.runtime;

public final class CatalaPosition
        extends CatalaValue<CatalaPosition> {

    public String filename;
    public int startLine;
    public int startColumn;
    public int endLine;
    public int endColumn;
    public String[] law_headings;

    public CatalaPosition(String filename, int startLine, int startColumn, int endLine, int endColumn, String[] law_headings) {
        this.filename = filename;
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.endLine = endLine;
        this.endColumn = endColumn;
        this.law_headings = law_headings;
    }

    public final static CatalaPosition empty = new CatalaPosition("", 0, 0, 0, 0, new String[]{});

    @Override
    public String toString() {
        if (this.equalsTo(empty).asBoolean()) {
            return "";
        }
        return filename + ":" + startLine + "." + startColumn + "-" + endLine + "." + endColumn;
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaPosition o) {
        return CatalaBool.fromBoolean(this.filename.equals(o.filename) && this.startLine == o.startLine
                && this.startColumn == o.startColumn && this.endLine == o.endLine);
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaPosition o) {
        CatalaPosition op = (CatalaPosition) o;
        int cmp = this.filename.compareTo(op.filename);
        if (cmp != 0) {
            return cmp;
        }
        cmp = Integer.compare(this.startLine, op.startLine);
        if (cmp != 0) {
            return cmp;
        }
        cmp = Integer.compare(this.endLine, op.endLine);
        if (cmp != 0) {
            return cmp;
        }
        cmp = Integer.compare(this.startColumn, op.startColumn);
        if (cmp != 0) {
            return cmp;
        }
        return Integer.compare(this.endColumn, op.endColumn);
    }

}

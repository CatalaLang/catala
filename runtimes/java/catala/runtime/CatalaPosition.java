package catala.runtime;

public final class CatalaPosition
        extends CatalaValue<CatalaPosition> {

    public String filename;
    public int startLine;
    public int startColumn;
    public int endLine;
    public int endColumn;

    public CatalaPosition(String filename, int startLine, int startColumn, int endLine, int endColumn) {
        this.filename = filename;
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.endLine = endLine;
        this.endColumn = endColumn;
    }

    public final static CatalaPosition empty = new CatalaPosition("", 0, 0, 0, 0);

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaPosition o) {
        return CatalaBool.of(this.filename.equals(o.filename) && this.startLine == o.startLine
                && this.startColumn == o.startColumn && this.endLine == o.endLine);
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaPosition o) {
        int cmp = this.filename.compareTo(o.filename);
        if (cmp != 0) {
            return cmp;
        }
        cmp = Integer.compare(this.startLine, o.startLine);
        if (cmp != 0) {
            return cmp;
        }
        cmp = Integer.compare(this.endLine, o.endLine);
        if (cmp != 0) {
            return cmp;
        }
        cmp = Integer.compare(this.startColumn, o.startColumn);
        if (cmp != 0) {
            return cmp;
        }
        return Integer.compare(this.endColumn, o.endColumn);
    }

    @Override
    public String toString() {
        if (this.equalsTo(empty).asBoolean()) {
            return "";
        }
        return filename + ":" + startLine + "." + startColumn + "-" + endLine + "." + endColumn;
    }

    @Override
    public String toJSONString() {
        StringBuilder b = new StringBuilder();
        java.util.function.BiConsumer<Integer, Integer> p_pos = (l, c) -> {
            b.append("{\"line\":").append(l).append(", ")
                    .append("\"character\":").append(c).append("}");
        };
        b.append("{ \"file\":\"").append(this.filename).append("\",\"start\":");
        p_pos.accept(this.startLine, this.startColumn);
        b.append(",\"end\":");
        p_pos.accept(this.endLine, this.endColumn);
        return b.append("}").toString();
    }
}

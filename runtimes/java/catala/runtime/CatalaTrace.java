package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.util.ArrayList;
import java.util.List;

public class CatalaTrace {

    public TraceNode currentNode;
    public List<TraceNode> rootNodes;
    public Boolean exceptionHandled;

    private static CatalaTrace instance = new CatalaTrace();

    private CatalaTrace() {
        this.rootNodes = new ArrayList<>();
        this.exceptionHandled = false;
    }

    public static void begin(TraceKind k, CatalaPosition pos) {
        instance.beginTrace(k, pos);
    }

    public static void end(CatalaValue<?> v) {
        instance.endTrace(v);
    }

    public static void end() {
        instance.endTrace();
    }

    public static void single(TraceKind k, CatalaPosition pos) {
        instance.singleTrace(k, pos);
    }

    public static void error(CatalaError.Error err, CatalaPosition pos,
            CatalaPosition[] relatedPos, String message) {
        if (message == null){
            single(new Error(err, relatedPos), pos);
        } else {
            single(new Error(err, relatedPos, message), pos);
        }
    }

    public static void error(CatalaError.Error e, List<CatalaPosition> lpos) {
        final CatalaPosition pos;
        CatalaPosition[] relatedPos = null;
        if (lpos.isEmpty()) {
            pos = CatalaPosition.empty;
        } else {
            pos = lpos.get(0);
            if (lpos.size() > 1) {
                relatedPos = (CatalaPosition[]) (lpos.subList(1, lpos.size()).toArray());
            }
        }
        error(e, pos, relatedPos, null);
    }

    public static void error(CatalaError.Error e, CatalaPosition pos) {
        error(e, pos, null, null);
    }

    public static void reset() {
        instance = new CatalaTrace();
    }

    public static CatalaTrace retrieve() {
        while (instance.currentNode != null) {
            end();
        }
        return instance;
    }

    private void beginTrace(TraceKind k, CatalaPosition pos) {
        TraceNode newNode = new TraceNode(k, pos, this.currentNode);
        if (this.currentNode == null) {
            // Root node
            this.rootNodes.add(newNode);
        } else {
            TraceNode parentNode = this.currentNode;
            parentNode.addSubNode(newNode);
        }
        this.currentNode = newNode;
    }

    private void endTrace(CatalaValue<?> v) {
        if (this.currentNode != null) {
            this.currentNode.v = v;
            this.currentNode = this.currentNode.parent;
        }
    }

    private void endTrace() {
        this.endTrace(null);
    }

    private void singleTrace(TraceKind k, CatalaPosition pos) {
        beginTrace(k, pos);
        endTrace();
    }

    public class TraceNode {

        final TraceKind kind;
        final CatalaPosition pos;
        private final TraceNode parent;
        CatalaValue<?> v;
        List<TraceNode> subNodes;

        TraceNode(TraceKind kind, CatalaPosition pos, TraceNode parent) {
            this.kind = kind;
            this.pos = pos;
            this.parent = parent;
        }

        void addSubNode(TraceNode n) {
            if (subNodes == null) {
                subNodes = new ArrayList<>();
            }
            subNodes.add(n);
        }

        void writeJSON(StringBuilder sb) {
            sb.append('{');
            sb.append("\"element\":");
            kind.writeJSON(sb);
            sb.append(',');
            sb.append("\"pos\":");
            sb.append(pos.toJSONString().replaceAll("\\s", ""));
            if (v != null) {
                sb.append(",\"value\":");
                sb.append(v.toJSONString().replaceAll("\\s", ""));
            }
            if (subNodes != null && !subNodes.isEmpty()) {
                sb.append(",\"trace\":[");
                int size = subNodes.size();
                for (int i = 0; i < size; i++) {
                    TraceNode n = subNodes.get(i);
                    n.writeJSON(sb);
                    if (i < size - 1) {
                        sb.append(',');
                    }
                }
                sb.append("]");
            }
            sb.append('}');
        }
    }

    public String toJSONString(StringBuilder sb) {
        sb.append('[');
        int size = rootNodes.size();
        for (int i = 0; i < size; i++) {
            TraceNode n = rootNodes.get(i);
            n.writeJSON(sb);
            if (i < size - 1) {
                sb.append(',');
            }
        }
        sb.append(']');
        return sb.toString();
    }

    public String toJSONString() {
        StringBuilder sb = new StringBuilder();
        return toJSONString(sb);
    }

    public interface TraceKind {

        public void writeJSON(StringBuilder sb);
    }

    public static class VarDef {

        public final String varName;
        public final CatalaPosition decl_pos;

        public VarDef(String varName, CatalaPosition decl_pos) {
            this.varName = varName;
            this.decl_pos = decl_pos;
        }

        void appendJSON(StringBuilder sb) {
            sb.append(String.format(",\"name\":\"%s\",\"decl_pos\":", varName));
            sb.append(decl_pos.toJSONString().replaceAll("\\s", ""));
        }
    }

    public static class ScopeCall implements TraceKind {

        public final VarDef scopeVar;

        public ScopeCall(String scopeName, CatalaPosition decl_pos) {
            this.scopeVar = new VarDef(scopeName, decl_pos);
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append("{\"kind\":\"scope_call\"");
            scopeVar.appendJSON(sb);
            sb.append("}");
        }
    }

    public static enum Input {
        NoInput, OnlyInput, Reentrant
    }

    public static String inputToString(Input i) {
        switch (i) {
            case NoInput:
                return "no_input";
            case OnlyInput:
                return "only_input";
            case Reentrant:
                return "reentrant";
        }
        return null;
    }

    public static class ScopeVarDef implements TraceKind {

        public final VarDef varDef;
        public final Input input;
        public final boolean output;

        public ScopeVarDef(String varName, CatalaPosition declPos, Input input, Boolean output) {
            this.varDef = new VarDef(varName, declPos);
            this.input = input;
            this.output = output;
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append("{\"kind\":\"scope_var\"");
            varDef.appendJSON(sb);
            sb.append(",\"input\":\"");
            sb.append(inputToString(input));
            sb.append(String.format("\",\"output\":%b", output));
            sb.append("}");
        }
    }

    public static class LocalVarDef implements TraceKind {

        public final String name;

        public LocalVarDef(String varName) {
            this.name = varName;
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append("{\"kind\":\"local_var\",\"name\":\"");
            sb.append(name);
            sb.append("\"}");
        }
    }

    public static class LocalTupDef implements TraceKind {

        public final String[] names;

        public LocalTupDef(String[] names) {
            this.names = names;
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append("{\"kind\":\"local_tup\",\"names\":[");
            for (int i = 0; i < names.length; i++) {
                sb.append(String.format("\"%s\"", names[i]));
                if (i < names.length - 1) {
                    sb.append(',');
                }
            }
            sb.append("]}");
        }
    }

    public static class FunCall implements TraceKind {

        public final VarDef varDef;

        public FunCall(String funcName, CatalaPosition declPos) {
            this.varDef = new VarDef(funcName, declPos);
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append("{\"kind\":\"function_call\"");
            varDef.appendJSON(sb);
            sb.append("}");
        }
    }

    public static abstract class AtomKind implements TraceKind {

        public final String kind;

        public AtomKind(String kind) {
            this.kind = kind;
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append(String.format("{\"kind\":\"%s\"}", kind));
        }
    }

    public static class BranchingCondition extends AtomKind {

        public BranchingCondition() {
            super("branch_condition");
        }

    }

    public static class IfBranching extends AtomKind {

        public IfBranching() {
            super("if_branching");
        }

    }

    public static class Assertion extends AtomKind {

        public Assertion() {
            super("assertion");
        }

    }

    public static class MatchBranching implements TraceKind {

        public final String constructorName;

        public MatchBranching(String constructorName) {
            this.constructorName = constructorName;
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append(String.format("{\"kind\":\"match_branching\",\"constructor\":\"%s\"}", constructorName));
        }
    }

    public static class Exception implements TraceKind {

        public final String label;
        public final CatalaPosition label_pos;
        public final CatalaPosition consequence_pos;

        public Exception(CatalaPosition consequence_pos) {
            this.label = null;
            this.label_pos = null;
            this.consequence_pos = consequence_pos;
        }

        public Exception(String label, CatalaPosition label_pos, CatalaPosition consequence_pos) {
            this.label = label;
            this.label_pos = label_pos;
            this.consequence_pos = consequence_pos;
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append("{\"kind\":\"exception\",");
            if (this.label != null && this.label_pos != null) {
                sb.append(String.format("\"label\":\"%s\",\"pos\":%s,",
                        label, label_pos.toJSONString().replaceAll("\\s", "")));
            }
            sb.append(String.format("\"cons_pos\":%s}", consequence_pos.toJSONString().replaceAll("\\s", "")));
        }
    }

    public static class Error implements TraceKind {

        public final CatalaError.Error error;
        public final CatalaPosition[] relatedPos;
        public final String message;

        public Error(CatalaError.Error error, CatalaPosition[] relatedPos, String message) {
            this.error = error;
            this.relatedPos = relatedPos;
            this.message = message;
        }

        public Error(CatalaError.Error error, CatalaPosition[] relatedPos) {
            this(error, relatedPos, error.toString());
        }

        @Override
        public void writeJSON(StringBuilder sb) {
            sb.append(String.format("{\"kind\":\"error\",\"type\":\"%s\",\"message\":\"%s\"",
                    error.name(), message));
            if (relatedPos != null && relatedPos.length > 0) {
                sb.append(',');
                sb.append("\"related_pos\":[");
                for (int i = 0; i < relatedPos.length; i++) {
                    sb.append(relatedPos[i].toJSONString().replaceAll("\\s", ""));
                    if (i < relatedPos.length - 1) {
                        sb.append(',');
                    }
                }
                sb.append(']');
            }
            sb.append('}');
        }
    }
}

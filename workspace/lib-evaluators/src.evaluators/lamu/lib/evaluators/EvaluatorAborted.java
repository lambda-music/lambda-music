package lamu.lib.evaluators;

import gnu.mapping.Values;

public class EvaluatorAborted extends RuntimeException {
    private final Object value;
    public Object getValue() {
        return value;
    }
    public EvaluatorAborted() {
        this.value = Values.empty;
    }
    public EvaluatorAborted(Object value) {
        super();
        this.value = value;
    }
}

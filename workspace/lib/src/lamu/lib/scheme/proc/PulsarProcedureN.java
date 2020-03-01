package lamu.lib.scheme.proc;

import java.lang.invoke.MethodHandle;

public abstract class PulsarProcedureN extends SafeProcedureN implements MultipleNamed {
    public PulsarProcedureN() {
        super();
    }
    public PulsarProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod, String n) {
        super( resultGoesToConsumer, applyMethod, n );
    }
    public PulsarProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod) {
        super( resultGoesToConsumer, applyMethod );
    }
    public PulsarProcedureN(MethodHandle applyMethod, String n) {
        super( applyMethod, n );
    }
    public PulsarProcedureN(MethodHandle applyMethod) {
        super( applyMethod );
    }
    public PulsarProcedureN( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

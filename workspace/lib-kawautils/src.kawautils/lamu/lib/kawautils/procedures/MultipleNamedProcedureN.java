package lamu.lib.kawautils.procedures;

import java.lang.invoke.MethodHandle;

public abstract class MultipleNamedProcedureN extends SafeProcedureN implements MultipleNamed {
    public MultipleNamedProcedureN() {
        super();
    }
    public MultipleNamedProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod, String n) {
        super( resultGoesToConsumer, applyMethod, n );
    }
    public MultipleNamedProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod) {
        super( resultGoesToConsumer, applyMethod );
    }
    public MultipleNamedProcedureN(MethodHandle applyMethod, String n) {
        super( applyMethod, n );
    }
    public MultipleNamedProcedureN(MethodHandle applyMethod) {
        super( applyMethod );
    }
    public MultipleNamedProcedureN( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

package pulsar;

import gnu.lists.LList;
import pulsar.lib.scheme.SafeProcedureN;

final class SchemeSequenceDefaultProcedure extends SafeProcedureN {
    private final LList notations;
    SchemeSequenceDefaultProcedure(LList notations) {
        this.notations = notations;
    }
    
    @Override
    public Object applyN(Object[] args) throws Throwable {
        return notations;
    }
}
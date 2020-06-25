package pulsar;

import gnu.lists.LList;
import gnu.mapping.Procedure;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure0;
import lamu.lib.threads.LamuThreadLocalInitializer;

final class ConstantListProcedure extends MultipleNamedProcedure0 {
    private final LList pair;
    public ConstantListProcedure( LList pair ) {
        this.pair = pair;
    }
    @Override
    public Object apply0() throws Throwable {
        return pair;
    }
}

final class DynamicPulsarProcedureFactory implements PulsarProcedureFactory {
    private final LamuThreadLocalInitializer threadLocalInitializer;
    private final Procedure procedure;
    public DynamicPulsarProcedureFactory(Procedure procedure) {
        this.threadLocalInitializer = new LamuThreadLocalInitializer();
        // Examine the specified procedure to check if it returns a procedure;
        // otherwise, raise an exception.
        try {
            Object object = procedure.apply0();
            if (!(object instanceof Procedure) ) {
                throw new IllegalArgumentException( "the procedure must return a procedure object" );
            }
        } catch ( Throwable t ) {
            throw new IllegalArgumentException( "the procedure throws an exception", t );
        }

        this.procedure = procedure;
    }
    @Override
    public Procedure createProcedure() throws IllegalStateException {
        this.threadLocalInitializer.restore();
        
        Procedure newp;
        try {
            newp = (Procedure) procedure.apply0();
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
        return newp;
    }
    @Override
    public String toString() {
        return String.format("(dynamic-procedure-factory proc: %s)", procedure );
    }
}

final class ConstantPulsarProcedureFactory implements PulsarProcedureFactory {
    private final LamuThreadLocalInitializer threadLocalInitializer;
    private final Procedure procedure;
    public ConstantPulsarProcedureFactory(Procedure procedure) {
        this.threadLocalInitializer = new LamuThreadLocalInitializer();
        this.procedure = procedure;
    }
    @Override
    public Procedure createProcedure() throws IllegalStateException {
        this.threadLocalInitializer.restore();
        return procedure;
    }
    @Override
    public String toString() {
        return String.format("(const-procedure-factory proc: %s)", procedure);
    }
}

public interface PulsarProcedureFactory {
    public static PulsarProcedureFactory createDynamic( Procedure procedure ) {
        return new DynamicPulsarProcedureFactory(procedure);
    }
    public static PulsarProcedureFactory createConstant( Procedure procedure ) {
        return new ConstantPulsarProcedureFactory(procedure);
    }
    public static Procedure createConstantListProcedure(LList pair) {
        return new ConstantListProcedure(pair);
    }
    public static PulsarProcedureFactory createConstantList(LList pair) {
        return createConstant( createConstantListProcedure(pair) );
    }
    Procedure createProcedure() throws IllegalStateException;
}

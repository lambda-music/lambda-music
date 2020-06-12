package pulsar;

import gnu.mapping.Procedure;
import lamu.lib.threads.LamuThreadLocalInitializer;
import metro.MetroSequence;
import metro.MetroSequenceFactory;

public final class SchemeSequenceFactory implements MetroSequenceFactory {
    /**
     * Create a factory instance by a factory object which generates a procedure.
     * 
     * @param procedureFactory
     * @return
     */
    public static SchemeSequenceFactory create( PulsarProcedureFactory procedureFactory ) {
        return new SchemeSequenceFactory(procedureFactory);
    }
    
    /**
     * 
     * @param procedure
     * @return
     */
    public static MetroSequenceFactory createDynamic( Procedure procedure ){
        return create( PulsarProcedureFactory.createDynamic( procedure ) );
    }
    /**
     * 
     * @param procedure
     * @return
     */
    public static MetroSequenceFactory createConstant( Procedure procedure ){
        return create( PulsarProcedureFactory.createConstant( procedure ) );
    }
    
    private final LamuThreadLocalInitializer threadLocalInitializer;
    private final PulsarProcedureFactory procedureFactory;
    private SchemeSequenceFactory( PulsarProcedureFactory procedureFactory ) {
        this.threadLocalInitializer = new LamuThreadLocalInitializer();
        this.procedureFactory = procedureFactory;
    }
    @Override
    public MetroSequence createSequence() {
        this.threadLocalInitializer.restore();
        Procedure procedure = procedureFactory.createProcedure();
        return new SchemeSequence( this.threadLocalInitializer, procedure );
    }
    
    @Override
    public String toString() {
        return String.format( "(const-sequence-factory value: %s)" , procedureFactory );
    }

}
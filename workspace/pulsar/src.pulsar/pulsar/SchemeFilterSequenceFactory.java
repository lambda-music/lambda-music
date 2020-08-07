package pulsar;

import gnu.mapping.Procedure;
import lamu.lib.kawautils.SchemeInvokable;
import lamu.lib.threads.LamuThreadLocalInitializer;
import metro.MetroPortSelector;
import metro.MetroSequence;
import metro.MetroSequenceFactory;

public final class SchemeFilterSequenceFactory implements MetroSequenceFactory {
    /**
     * Create a factory instance by a factory object which generates a procedure.
     * 
     * @param procedureFactory
     * @return
     */
    public static SchemeFilterSequenceFactory create(MetroPortSelector portSelector, PulsarProcedureFactory procedureFactory ) {
        return new SchemeFilterSequenceFactory(portSelector, procedureFactory);
    }
    
    /**
     * 
     * @param procedure
     * @return
     */
    public static MetroSequenceFactory createDynamic(MetroPortSelector portSelector,  Procedure procedure ){
        return create( portSelector, PulsarProcedureFactory.createDynamic( procedure ) );
    }
    /**
     * 
     * @param procedure
     * @return
     */
    public static MetroSequenceFactory createConstant(MetroPortSelector portSelector, Procedure procedure ){
        return create( portSelector, PulsarProcedureFactory.createConstant( procedure ) );
    }
    
    private final MetroPortSelector portSelector;
    private final LamuThreadLocalInitializer threadLocalInitializer;
    private final PulsarProcedureFactory procedureFactory;
    private SchemeFilterSequenceFactory( MetroPortSelector portSelector, PulsarProcedureFactory procedureFactory ) {
        this.portSelector = portSelector;
        this.threadLocalInitializer = new LamuThreadLocalInitializer();
        this.procedureFactory = procedureFactory;
    }
    @Override
    public MetroSequence createSequence() {
        this.threadLocalInitializer.restore();
        Procedure procedure = procedureFactory.createProcedure();
        return new SchemeFilterSequence( 
            portSelector, 
            this.threadLocalInitializer,
            SchemeInvokable.create(procedure));
    }
    
    @Override
    public String toString() {
        return String.format( "(const-sequence-factory value: %s)" , procedureFactory );
    }

}
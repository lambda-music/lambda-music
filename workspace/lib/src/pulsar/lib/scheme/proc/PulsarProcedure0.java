package pulsar.lib.scheme.proc;

import gnu.mapping.Procedure0;

public abstract class PulsarProcedure0 extends Procedure0 implements MultipleNamed {
    public PulsarProcedure0() {
        super();
    }
    public PulsarProcedure0(String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

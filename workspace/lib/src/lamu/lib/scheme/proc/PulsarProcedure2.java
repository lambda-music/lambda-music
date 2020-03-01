package lamu.lib.scheme.proc;

import gnu.mapping.Procedure2;

public abstract class PulsarProcedure2 extends Procedure2 implements MultipleNamed {
    public PulsarProcedure2() {
        super();
    }

    public PulsarProcedure2( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

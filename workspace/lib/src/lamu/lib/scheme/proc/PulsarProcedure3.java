package lamu.lib.scheme.proc;

import gnu.mapping.Procedure3;

public abstract class PulsarProcedure3 extends Procedure3 implements MultipleNamed {
    public PulsarProcedure3() {
        super();
    }

    public PulsarProcedure3( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

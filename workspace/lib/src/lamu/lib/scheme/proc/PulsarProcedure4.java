package lamu.lib.scheme.proc;

import gnu.mapping.Procedure4;

public abstract class PulsarProcedure4 extends Procedure4 implements MultipleNamed {
    public PulsarProcedure4() {
        super();
    }

    public PulsarProcedure4( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

package lamu.lib.scheme.proc;

import gnu.mapping.Procedure0or1;

public abstract class PulsarProcedure0or1 extends Procedure0or1 implements MultipleNamed {
    public PulsarProcedure0or1() {
        super();
    }

    public PulsarProcedure0or1(String ... names) {
        super( names[0] );
        setNames( names );
    }
}

package lamu.lib.scheme.proc;

import gnu.mapping.Procedure0;

public abstract class MultipleNamedProcedure0 extends Procedure0 implements MultipleNamed {
    public MultipleNamedProcedure0() {
        super();
    }
    public MultipleNamedProcedure0(String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

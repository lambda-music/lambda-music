package lamu.lib.scheme.proc;

import gnu.mapping.Procedure3;

public abstract class MultipleNamedProcedure3 extends Procedure3 implements MultipleNamed {
    public MultipleNamedProcedure3() {
        super();
    }

    public MultipleNamedProcedure3( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

package quartz.lib.scheme.proc;

import gnu.mapping.Procedure2;

public abstract class MultipleNamedProcedure2 extends Procedure2 implements MultipleNamed {
    public MultipleNamedProcedure2() {
        super();
    }

    public MultipleNamedProcedure2( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

package quartz.lib.scheme.proc;

import gnu.mapping.Procedure0or1;

public abstract class MultipleNamedProcedure0or1 extends Procedure0or1 implements MultipleNamed {
    public MultipleNamedProcedure0or1() {
        super();
    }

    public MultipleNamedProcedure0or1(String ... names) {
        super( names[0] );
        setNames( names );
    }
}

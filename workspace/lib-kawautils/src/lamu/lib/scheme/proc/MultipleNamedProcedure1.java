package lamu.lib.scheme.proc;

import gnu.mapping.Procedure1;

public abstract class MultipleNamedProcedure1 extends Procedure1 implements MultipleNamed {
    public MultipleNamedProcedure1() {
        super();
    }

    public MultipleNamedProcedure1(String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

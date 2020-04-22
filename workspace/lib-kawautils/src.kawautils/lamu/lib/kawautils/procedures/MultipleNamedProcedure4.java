package lamu.lib.kawautils.procedures;

import gnu.mapping.Procedure4;

public abstract class MultipleNamedProcedure4 extends Procedure4 implements MultipleNamed {
    public MultipleNamedProcedure4() {
        super();
    }

    public MultipleNamedProcedure4( String ... names ) {
        super( names[0] );
        setNames( names );
    }
}

package lamu.lib.kawautils.procedures;

import gnu.mapping.Procedure1or2;

public abstract class MultipleNamedProcedure1or2 extends Procedure1or2 implements MultipleNamed {
    public MultipleNamedProcedure1or2() {
        super();
    }

    public MultipleNamedProcedure1or2(String ... names) {
        super( names[0] );
        setNames( names );
    }
}

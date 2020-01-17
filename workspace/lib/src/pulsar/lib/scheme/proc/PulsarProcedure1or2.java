package pulsar.lib.scheme.proc;

import gnu.mapping.Procedure1or2;

public abstract class PulsarProcedure1or2 extends Procedure1or2 implements MultipleNamed {
    public PulsarProcedure1or2() {
        super();
    }

    public PulsarProcedure1or2(String name) {
        super( name );
    }
}

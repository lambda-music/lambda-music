package pulsar.lib.scheme.proc;

import gnu.mapping.Procedure1;

public abstract class PulsarProcedure1 extends Procedure1 implements MultipleNamed {
    public PulsarProcedure1() {
        super();
    }

    public PulsarProcedure1(String name) {
        super( name );
    }
}

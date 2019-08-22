package pulsar.lib.scheme;

import gnu.mapping.Procedure1or2;

public abstract class DProcedure1or2 extends Procedure1or2 implements DescriptiveProcedure {
	public DProcedure1or2() {
		super();
	}
	public DProcedure1or2(String name) {
		super(name);
	}
}

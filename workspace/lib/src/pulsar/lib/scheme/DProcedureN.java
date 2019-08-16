package pulsar.lib.scheme;

import java.lang.invoke.MethodHandle;

import gnu.mapping.ProcedureN;

public abstract class DProcedureN extends ProcedureN implements DescriptiveProcedure {
	public DProcedureN() {
	}
	public DProcedureN(String name) {
		super(name);
	}
	public DProcedureN(MethodHandle applyMethod) {
		super(applyMethod);
	}
	public DProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod) {
		super(resultGoesToConsumer, applyMethod);
	}
	public DProcedureN(MethodHandle applyMethod, String n) {
		super(applyMethod, n);
	}
	public DProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod, String n) {
		super(resultGoesToConsumer, applyMethod, n);
	}
}

package pulsar.lib.scheme;

import java.lang.invoke.MethodHandle;

import gnu.mapping.ProcedureN;

/**
 * If you forget to override {@link ProcedureN#applyN(Object[])}, you'll get a mysterious
 * infinite recursion error which cost you a couple of hours to find out. In order to prevent
 * this error, I will make it an abstract method and ban to use the ProcedureN directly.
 * (Thu, 29 Aug 2019 19:08:52 +0900)
 */
public abstract class SafeProcedureN extends ProcedureN {
	public SafeProcedureN() {
		super();
	}
	public SafeProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod, String n) {
		super( resultGoesToConsumer, applyMethod, n );
	}
	public SafeProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod) {
		super( resultGoesToConsumer, applyMethod );
	}
	public SafeProcedureN(MethodHandle applyMethod, String n) {
		super( applyMethod, n );
	}
	public SafeProcedureN(MethodHandle applyMethod) {
		super( applyMethod );
	}
	public SafeProcedureN(String name) {
		super( name );
	}
	@Override
	public abstract Object applyN(Object[] args) throws Throwable;
}

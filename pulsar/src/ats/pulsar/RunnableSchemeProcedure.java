package ats.pulsar;

public final class RunnableSchemeProcedure implements Runnable {
	final InvocableSchemeProcedure procedure;
	final Object[] args;
	public RunnableSchemeProcedure( InvocableSchemeProcedure procedure , Object ... args ) {
		this.procedure = procedure;
		this.args = args;
	}
	@Override
	public void run() {
		procedure.invoke(args);
	}
}
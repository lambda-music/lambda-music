package ats.pulsar;

final class RunnableSchemeProcedure implements Runnable {
	final InvocableSchemeProcedure procedure;
	final Object[] args;
	RunnableSchemeProcedure( InvocableSchemeProcedure procedure , Object ... args ) {
		this.procedure = procedure;
		this.args = args;
	}
	@Override
	public void run() {
		procedure.invoke(args);
	}
}
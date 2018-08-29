package ats.pulsar;

final class RunnableSchemeProcedure implements Runnable {
	final InvokableSchemeProcedure procedure;
	final Object[] args;
	RunnableSchemeProcedure( InvokableSchemeProcedure procedure , Object ... args ) {
		this.procedure = procedure;
		this.args = args;
	}
	@Override
	public void run() {
		procedure.invoke(args);
	}
}
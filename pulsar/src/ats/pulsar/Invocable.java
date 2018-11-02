package ats.pulsar;

/**
 * A interface that defined objects to execute (currently only ) scheme functions.  
 */
public abstract interface Invocable {
	static final Object[] NULL_ARG = {};
	abstract Object invoke( Object ... args );
}
package ats.metro;

/**
 * A interface that defined objects to execute (currently only ) scheme functions.  
 */
public abstract interface MetroInvokable {
	static final Object[] NULL_ARG = {};
	abstract Object invoke( Object ... args );
}
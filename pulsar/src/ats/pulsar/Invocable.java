package ats.pulsar;

/**
 * This class defines an interface that executes an arbitrary procedure.
 * Currently there is only one class which implements this interface. See
 * {@link InvocableSchemeProcedure}
 * 
 * @author ats
 */
public abstract interface Invocable {
	/**
	 * This method invokes the procedure which is denoted by the subclasses
	 * implement this interface.
	 * 
	 * @param args
	 * @return
	 */
	abstract Object invoke(Object... args);
}
package ats.pulsar;

import gnu.mapping.Environment;
import gnu.mapping.Procedure;

/**
 * This class implements invocation of procedures of a Kawa scheme.
 * 
 * In this application, every exceptions are merely printed in standard error
 * stream and these exceptions are not handled. When any exception are thrown,
 * the processing just continues and will not interrupt.
 * 
 * This behavior is currently intentional because many processing are done in
 * multithreading and also time-critical since this application works with
 * JACKaudio.
 * 
 * These exceptions should be displayed not only in the standard error stream,
 * but also in balloons in notification area,etc something which are easily
 * visible to users. Some new systems are expected to be developed in future.
 * 
 * @author ats
 */
class InvocableSchemeProcedure implements Invocable {
	/**
	 * This field specifies an object to be synchronized with.
	 * This is necessary since Kawa is not very good at multithreading. 
	 */
	private final Object syncObj;
	
	/**
	 * This field specifies an environment object which contains a scope of scheme
	 * language to be executed with.
	 * 
	 * I think the necessity of this field is needed to be explained.
	 * 
	 * Usually these environment objects are not supposed to be set manually. The
	 * reason why it to be set manually is that sometimes the environment object is
	 * missing. I am still not sure what exactly condition causes it to be gone, but
	 * it occurs sometimes.
	 * 
	 * It seemed to me that Kawa's environment objects are stored per a thread. I am
	 * still not sure when they are created. But when multiple threads access to a
	 * single Kawa scheme object, sometime the environment object on the scheme
	 * object is gone and this phenomenon causes strange errors.
	 * 
	 * A working around which I found is setting the desired environment object
	 * before calling the procedure.
	 * 
	 * This field keeps the environment object which is in charge when the procedure
	 * (closure) was passed in order to set it it every time before calling the
	 * procedure.
	 * 
	 * I am quite sure this is not proper way to solve the problem, but it worked
	 * for me.
	 */
	private Environment environment;
	
	/**
	 * The procedure to invoke.
	 */
	private final Procedure procedure;

	InvocableSchemeProcedure(Object syncObj, Environment environment, Procedure procedure) {
		this.syncObj = syncObj;
		this.environment = environment;
		this.procedure = procedure;
	}

	@Override
	public Object invoke(Object... args) {
		synchronized (syncObj) {
			try {
				Environment.setCurrent(this.environment);
				return procedure.applyN(new Object[] {});
			} catch (Throwable e) {
				Pulsar.logError("SchemeInvokableProcedure", e);
				throw new RuntimeException(e);
			}
		}
	}
}
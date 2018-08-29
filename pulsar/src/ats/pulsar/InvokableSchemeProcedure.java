package ats.pulsar;

import ats.metro.MetroInvokable;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;

final class InvokableSchemeProcedure implements MetroInvokable {
	private final Object syncObj;
	private Environment environment;
	private final Procedure procedure;
//	InvokableSchemeProcedure( Object syncObj, Procedure procedure ) {
//		this( syncObj, Environment.getCurrent(), procedure );
//	}
	InvokableSchemeProcedure( Object syncObj, Environment environment, Procedure procedure ) {
		this.syncObj = syncObj;
		this.environment = environment;
		this.procedure = procedure;
	}
	@Override
	public Object invoke( Object... args ) {
		synchronized ( syncObj ) {
			try {
				Environment.setCurrent( this.environment );
				return procedure.applyN( new Object[] {} );
			} catch (Throwable e) {
				Pulsar.logError( "SchemeInvokableProcedure" , e );
				throw new RuntimeException(e);
			}
		}
	}
}
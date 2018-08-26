package ats.metro;

import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.mapping.Environment;
import gnu.mapping.Procedure;

public class MetroSchemeProcedureEvent extends MetroEvent {
	private final Environment environment;
	private final Procedure procedure;
	private final class RunnableSchemeProcedure implements Runnable {
		@Override
		public void run() {
			try {
				Environment.setCurrent( environment );
				procedure.applyN( new Object[] {} );
			} catch (Throwable e) {
		        Logger.getLogger(Metro.class.getName()).log(Level.SEVERE, null, e);
				throw new RuntimeException(e);
			}
		}
	}
	public MetroSchemeProcedureEvent(double offset, Environment environment, Procedure procedure ) {
		super(offset);
		this.environment = environment;
		this.procedure = procedure;
	}
	
	public void execute( Metro metro ) {
		metro.postMessage( new RunnableSchemeProcedure() );
	}
}

package ats.metro;

public class MetroSchemeProcedureEvent extends MetroEvent {
	private final Runnable runnable;
	public MetroSchemeProcedureEvent(double offset, Runnable runnable ) {
		super(offset);
		this.runnable = runnable;
	}
	public void execute( Metro metro ) {
		metro.postMessage( runnable );
	}
}

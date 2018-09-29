package ats.metro;

public class MetroAbstractSchemeProcedureEvent extends MetroAbstractEvent {
	private final Runnable runnable;
	public MetroAbstractSchemeProcedureEvent(double offset, Runnable runnable ) {
		super(offset);
		this.runnable = runnable;
	}
	public void execute( Metro metro ) {
		metro.postMessage( runnable );
	}
}

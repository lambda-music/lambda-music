package ats.metro;

import java.util.List;

public class MetroAbstractSchemeProcedureEvent extends MetroAbstractEvent {
	private final Runnable runnable;
	public MetroAbstractSchemeProcedureEvent(double offset, Runnable runnable ) {
		super(offset);
		this.runnable = runnable;
	}
	public void execute( Metro metro ) {
		metro.postMessage( runnable );
	}
	@Override
	public void process(Metro metro, int from, int to, int nframes, List<AbstractMidiEvent> eventList) {
		execute( metro );		
	}
}

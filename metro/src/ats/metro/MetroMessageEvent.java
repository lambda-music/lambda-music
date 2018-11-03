package ats.metro;

import java.util.List;

public class MetroMessageEvent extends MetroEvent {
	private final Runnable message;
	public MetroMessageEvent(double offset, Runnable message ) {
		super(offset);
		this.message = message;
	}
	public void execute( Metro metro ) {
		metro.postMessage( message );
	}
	@Override
	public void process(Metro metro, int from, int to, int nframes, List<MetroAbstractMidiEvent> eventList) {
		execute( metro );		
	}
}

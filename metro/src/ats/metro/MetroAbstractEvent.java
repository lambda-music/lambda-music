package ats.metro;

import java.util.List;

public interface MetroAbstractEvent {
	boolean between(int from, int to);
	void process(Metro metro, int from, int to, int nframes, List<MetroAbstractMidiEvent> eventList);
}
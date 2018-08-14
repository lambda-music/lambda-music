package ats.pulsar;

import ats.metro.MetroNoteEventBuffer;

public interface Pulsable {
	void pulse( MetroNoteEventBuffer buf );
	double getBars();
}

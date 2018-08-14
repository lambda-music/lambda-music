package ats.pulsar;

import ats.metro.MetroMidiEventBuffer;

public interface Pulsable {
	void pulse( MetroMidiEventBuffer buf );
	double getBars();
}

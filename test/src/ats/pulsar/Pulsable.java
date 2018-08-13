package ats.pulsar;

import nu.oka.metro.MetroMidiEventBuffer;

public interface Pulsable {
	void pulse( MetroMidiEventBuffer buf );
	double getBars();
}

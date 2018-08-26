package ats.pulsar.old;

import ats.metro.Metro;
import ats.metro.MetroNoteEventBuffer;
import ats.metro.MetroNoteEventBufferSequence;

public interface Pulsable {
	void pulse( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf );
	double getBarLength();
}

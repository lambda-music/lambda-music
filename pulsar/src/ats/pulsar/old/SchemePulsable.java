package ats.pulsar.old;

import ats.metro.Metro;
import ats.metro.MetroNoteEventBuffer;
import ats.metro.MetroNoteEventBufferSequence;
import ats.pulsar.SchemePulsarLogic;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;

class SchemePulsable implements Pulsable {
	final double barLength;// this maybe unnecessary
	final Procedure procedure;
	public SchemePulsable(double barLength, Procedure procedure) {
		this.barLength = barLength;
		this.procedure = procedure;
	}
	@Override
	public void pulse( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf ) {
		SchemePulsarLogic.scheme2buf( metro, sequence, Environment.getCurrent(), procedure, buf);
	}

	@Override
	public double getBarLength() {
		return barLength;
	}
}
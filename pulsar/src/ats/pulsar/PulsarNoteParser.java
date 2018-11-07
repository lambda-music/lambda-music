package ats.pulsar;

import ats.metro.Metro;
import ats.metro.MetroEventBuffer;
import ats.metro.MetroTrack;
import gnu.lists.AbstractSequence;

public class PulsarNoteParser {
	static final NoteListParser PARSER = new NoteListParser();
	static {
		PARSER.putAll( MidiNoteListParsers.getElements() );
		PARSER.putAll( SpecialNoteListParsers.getElements() );
	}
	public static boolean parse( Metro metro, MetroTrack track, AbstractSequence<Object> inputList, MetroEventBuffer outputBuffer, boolean result ) {
		return PARSER.parse(metro, track, inputList, outputBuffer, result);
	}
}

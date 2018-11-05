package ats.pulsar;

import ats.metro.Metro;
import ats.metro.MetroNoteEventBuffer;
import gnu.lists.AbstractSequence;
import kawa.standard.Scheme;

public class PulsarNoteParser {
	static final NoteListParser PARSER = new NoteListParser();
	static {
		PARSER.putAllParsers( MidiNoteListParsers.getElements() );
		PARSER.putAllParsers( SpecialNoteListParsers.getElements() );
	}
	public static boolean parse( Metro metro, Scheme scheme, AbstractSequence<Object> inputList, MetroNoteEventBuffer outputBuffer, boolean result ) {
		return PARSER.parse(metro, scheme, inputList, outputBuffer, result);
	}
}

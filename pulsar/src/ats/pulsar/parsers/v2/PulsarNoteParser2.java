package ats.pulsar.parsers.v2;

import ats.pulsar.NoteListParser;

public class PulsarNoteParser2 extends NoteListParser {
	private static final PulsarNoteParser2 INSTANCE = new PulsarNoteParser2();
	private PulsarNoteParser2() {
		this.putAll( MidiNoteListParsers2.getElements() );
		this.putAll( SpecialNoteListParsers2.getElements() );
	}
	public static NoteListParser getInstance() {
		return INSTANCE;
	}
}

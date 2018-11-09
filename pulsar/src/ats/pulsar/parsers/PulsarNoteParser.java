package ats.pulsar.parsers;

import ats.pulsar.NoteListParser;

public class PulsarNoteParser extends NoteListParser {
	private static final PulsarNoteParser INSTANCE = new PulsarNoteParser();
	private PulsarNoteParser() {
		this.putAll( MidiNoteListParsers.getElements() );
		this.putAll( SpecialNoteListParsers.getElements() );
	}
	public static final PulsarNoteParser getInstance() {
		return INSTANCE;
	}
}

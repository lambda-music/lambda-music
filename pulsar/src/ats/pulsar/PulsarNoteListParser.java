package ats.pulsar;

public class PulsarNoteListParser extends NoteListParser {
	private static final PulsarNoteListParser INSTANCE = new PulsarNoteListParser();
	private PulsarNoteListParser() {
		this.putAll( PulsarMidiNoteListParsers.getElements() );
		this.putAll( PulsarSpecialNoteListParsers.getElements() );
	}
	public static final PulsarNoteListParser getInstance() {
		return INSTANCE;
	}
}

package pulsar;

import kawa.standard.Scheme;

public class PulsarDocuments {
	private PulsarDocuments() {}
	public static void defineDoc(Scheme scheme, NoteListParser parser ) {
		for ( NoteListParserElement element : parser.getAllElements() ) {
			defineDoc( scheme, element );
		}
	}
	public static void defineDoc(Scheme scheme, NoteListParserElement element ) {
		// XXX
	}
}

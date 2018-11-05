package ats.pulsar;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroNoteEventBuffer;
import ats.pulsar.MidiNoteListParsers.MidiNoteListParserElement;
import ats.pulsar.lib.SchemeUtils;
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import kawa.standard.Scheme;

public class NoteListParser {
	static final Logger LOGGER = Logger.getLogger(NoteListParser.class.getName());
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		// LOGGER.log(Level.INFO, msg);
		System.err.println(msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}

	public static final String ID_TYPE      = "type";
	
	private HashMap<String,NoteListParserElement> parserMap = new HashMap<String,NoteListParserElement>();
	public void putParser( NoteListParserElement info ) {
		String name = info.getShortName();
		if ( parserMap.containsKey(name) )
			throw new RuntimeException( "internal error : id (" + name + ") is already registered." );
		
		parserMap.put( name, info );
	}
	public void putAllParsers( Collection<NoteListParserElement> elements ) {
		for( NoteListParserElement e : elements ) {
			putParser( e );
		}
	}
	public NoteListParserElement getParser( String id ) {
		return parserMap.get(id);
	}
	
	public boolean parse( Metro metro, Scheme scheme, AbstractSequence<Object> inputList, MetroNoteEventBuffer outputBuffer, boolean result ) {
		// boolean result = true;
		if ( inputList != null ) {
			for ( Iterator<Object> i = inputList.iterator(); i.hasNext(); ) {
 				Object obj = i.next();
				if ( obj instanceof Pair ) {
					Pair record = (Pair)obj;
					result = parseNote( metro, scheme, outputBuffer, result, record );
				} else if ( obj instanceof Boolean ) {
					continue;
				} else {
					SchemePulsarLogic.LOGGER.log( Level.WARNING, "Unsupported object type was found. We ignored it." + obj );
				}
					
			} // end of the loop
		}
		// buf.setLength( this.bars );
		// buf.noteShot(0, 1, 0, 73, 100 );
		return result;
	}

	private boolean parseNote( Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, boolean result, Pair record ) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean parseNote( Metro metro, Scheme scheme, MetroNoteEventBuffer outputBuffer, boolean result, AbstractSequence list ) {
		Map<String,Object> map = SchemeUtils.list2map(list, null );
		String type      = map.containsKey( ID_TYPE ) ? SchemeUtils.symbolToString(  map.get( ID_TYPE ) ) : "";
		MidiNoteListParserElement parser = getParser( type );
		if ( parser == null ) {
			logWarn( "unknown type (" +  type + ")" );
			return result;
		} else {
			return parser.parseEvent( metro, scheme, outputBuffer, map, result );
		}
	}
}

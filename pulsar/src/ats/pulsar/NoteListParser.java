package ats.pulsar;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroEventBuffer;
import ats.metro.MetroTrack;
import ats.pulsar.lib.SchemeUtils;
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;

/**
 * {@link NoteListParser} receives a cons cell of Scheme language which contains
 * a list of association lists ( called <i>note list</i> ), and converts it to
 * MIDI data. The result of the conversion is sent to {@link MetroEventBuffer} .
 * 
 * An instance of {@link NoteListParser} contains a set of
 * {@link NoteListParserElement} objects. Each of the elements is responsible to
 * convert a type of notes and generates MIDI data.
 * 
 * @author ats
 */
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
	
	private HashMap<String,NoteListParserElement> shortNameMap = new HashMap<String,NoteListParserElement>();
	private HashMap<String,NoteListParserElement> longNameMap = new HashMap<String,NoteListParserElement>();

	private void addProc(HashMap<String, NoteListParserElement> map, String key, NoteListParserElement value ) {
		logInfo( shortNameMap.size() + " : putParser( " + key + " )" );
		if ( key == null || key.equals( "" ) ) {
			throw new RuntimeException( "internal error : " + value.getClass().getName() + " has no name. "  );
		}
		if ( shortNameMap.containsKey(key) )
			throw new RuntimeException( "internal error : id (" + key + ") is already registered." );
		
		map.put( key, value );
	}

	/**
	 * Add a parser element to this parser object. 
	 * @param e
	 *    A parser element object to add.
	 */
	public void put( NoteListParserElement e ) {
		addProc( shortNameMap, e.getShortName(), e );
		addProc( longNameMap, e.getLongName(), e );
	}
	/**
	 * Add all of parser elements in the given collection object.
	 * @param ce
	 *   a collection object contains the parser elmeents to add.
	 */
	public void putAll( Collection<NoteListParserElement> ce ) {
		for( NoteListParserElement e : ce ) {
			put( e );
		}
	}
	
	/**
	 * Get a parser element from this parser object. This method lookups mapping
	 * between short names and the elements at first, and then lookup the mapping
	 * between long names and the elements only if the former failed to find the
	 * element.
	 * 
	 * @param name
	 *     The element name as either a short name or a long name.  
	 * @return
	 *     The element or null if not found.
	 */
	public NoteListParserElement get( String name ) {
		return shortNameMap.containsKey( name ) ? shortNameMap.get(name) : longNameMap.get(name); 
	}
	
	/**
	 * This method parses the specified note list. 
	 * 
	 * Refer
	 * {@linkplain ats.pulsar.SchemeSequence#processBuffered(Metro, MetroTrack, MetroEventBuffer) processBuffered}
	 * to see how this method is called.
     * 
	 * @param metro
	 *            The instance of the current {@link Metro}.
	 * @param track
	 *            The instance of the current {@link MetroTrack} which generated the
	 *            note.
	 * @param buf
	 *            The instance of the current {@link MetroEventBuffer} to output
	 *            into as a result of the processing.
	 * @param result
	 *            The result of the previous parser element.
	 * @return Boolean value <code>true</code> if the current measure should
	 *         continue to play; otherwise return <code>false</code>.
	 *         <p>
	 *         If the parser does not have specific opinion whether it should
	 *         continue or not, returning the value of <code>result</code> parameter
	 *         is sufficient.
	 * @see ats.pulsar.NoteListParserElement#parseEvent(Metro, MetroTrack,
	 *      MetroEventBuffer, Map, boolean)
	 */
	public boolean parse( Metro metro, MetroTrack track, AbstractSequence<Object> inputList, MetroEventBuffer buf, boolean result ) {
		// boolean result = true;
		if ( inputList != null ) {
			for ( Iterator<Object> i = inputList.iterator(); i.hasNext(); ) {
 				Object obj = i.next();
				if ( obj instanceof Pair ) {
					Pair record = (Pair)obj;
					result = parseNote( metro, track, buf, result, record );
				} else if ( obj instanceof Boolean ) {
					continue;
				} else {
					LOGGER.log( Level.WARNING, "Unsupported object type was found. We ignored it." + obj );
				}
					
			} // end of the loop
		}
		return result;
	}

	private boolean parseNote( Metro metro, MetroTrack track, MetroEventBuffer outputBuffer, boolean result, AbstractSequence list ) {
		Map<String,Object> map = SchemeUtils.list2map(list, null );
		String type      = map.containsKey( ID_TYPE ) ? SchemeUtils.symbolToString(  map.get( ID_TYPE ) ) : "";
		NoteListParserElement parser = get( type );
		if ( parser == null ) {
			logWarn( "unknown type (" +  type + ")" );
			return result;
		} else {
			return parser.parseEvent( metro, track, outputBuffer, map, result );
		}
	}
}

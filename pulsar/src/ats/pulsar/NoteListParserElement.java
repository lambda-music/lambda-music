package ats.pulsar;

import java.util.Map;

import ats.metro.Metro;
import ats.metro.MetroEventBuffer;
import ats.metro.MetroTrack;

/**
 * An instance of {@link NoteListParserElement} is a representation of a type of
 * notes on the note list. This defines how a note element to be parsed and to
 * perform.
 * 
 * See {@link NoteListParser}.
 * 
 * @author ats
 */
public abstract class NoteListParserElement {
	/**
	 * Returns its long name of the target notes. The name should only contain
	 * English words without abbreviation. The word separator must only be hyphens.
	 * <p>
	 * This name should be a valid scheme identifier.
	 * 
	 * @return
	 *   The note type name.
	 */
	public abstract String getLongName();
	
	/**
	 * Returns its short name of the target notes. The name should be equal or
	 * shorter than four characters. This short name is applied as an identifier
	 * in a parser object; this should also be unique. 
	 * <p>
	 * This name should be a valid scheme identifier.
	 * 
	 * @return
	 *   The note type name in a short form.
	 */
	public abstract String getShortName();
	
	/**
	 * Parse the current note on the processing note list. For further information,
	 * see
	 * <ul>
	 * <li>
	 * {@link NoteListParser#parseNote(Metro, MetroTrack, MetroEventBuffer, boolean, gnu.lists.AbstractSequence) }
	 * </li>
	 * <li>
	 * {@link NoteListParser#parse(Metro, MetroTrack, gnu.lists.AbstractSequence, MetroEventBuffer, boolean) }
	 * </li>
	 * </ul>
	 * 
	 * @param metro
	 *            The instance of the current {@link Metro}.
	 * @param track
	 *            The instance of the current {@link MetroTrack} which generated the
	 *            note.
	 * @param buf
	 *            The instance of the current {@link MetroEventBuffer} to output
	 *            into as a result of the processing.
	 * @param map
	 *            An instance of {@link Map} that contains the contents of Scheme's
	 *            association list.
	 * @param result
	 *            The result of the previous parser element.
	 * @return Boolean value <code>true</code> if the current measure should
	 *         continue to play; otherwise return <code>false</code>.
	 *         <p>
	 *         If the parser does not have specific opinion whether it should
	 *         continue or not, returning the value of <code>result</code> parameter
	 *         is sufficient.
	 */
	public abstract boolean parseEvent( Metro metro, MetroTrack track, MetroEventBuffer buf, Map<String,Object> map, boolean result );
}
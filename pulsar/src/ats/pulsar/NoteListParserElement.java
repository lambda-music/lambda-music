package ats.pulsar;

import java.util.Map;

import ats.metro.Metro;
import ats.metro.MetroNoteEventBuffer;
import kawa.standard.Scheme;

public abstract class NoteListParserElement {
	public abstract String getLongName();
	public abstract String getShortName();
	public abstract boolean parseEvent( Metro metro, Scheme scheme, MetroNoteEventBuffer buf, Map<String,Object> map, boolean result );
}
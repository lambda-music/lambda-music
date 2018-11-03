package ats.metro;

import java.util.Comparator;

public interface MetroAbstractMidiEvent extends MetroAbstractEvent, Comparable<MetroAbstractMidiEvent> {
	public static final Comparator<? super MetroAbstractMidiEvent> COMPARATOR = new Comparator<MetroAbstractMidiEvent>( ) {
		@Override
		public int compare(MetroAbstractMidiEvent o1, MetroAbstractMidiEvent o2) {
			return o1.compareTo( o2 );
		}
	};

	int getOutputPortNo();
	int    getMidiOffset();
	byte[] getMidiData();
	
	public default int compareTo( MetroAbstractMidiEvent o ) {
		return this.getMidiOffset() - o.getMidiOffset();
	}
}

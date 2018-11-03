package ats.metro;

import java.util.Comparator;

public interface AbstractMidiEvent extends Comparable<AbstractMidiEvent> {
	public static final Comparator<? super AbstractMidiEvent> COMPARATOR = new Comparator<AbstractMidiEvent>( ) {
		@Override
		public int compare(AbstractMidiEvent o1, AbstractMidiEvent o2) {
			return o1.compareTo( o2 );
		}
	};

	int getOutputPortNo();
	int    getMidiOffset();
	byte[] getMidiData();
	
	public default int compareTo( AbstractMidiEvent o ) {
		return this.getMidiOffset() - o.getMidiOffset();
	}
}

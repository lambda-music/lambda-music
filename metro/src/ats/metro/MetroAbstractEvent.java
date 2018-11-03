package ats.metro;

import java.util.Comparator;
import java.util.List;

public abstract class MetroAbstractEvent {
//	public static final Comparator<? super MetroAbstractEvent> comparator = new Comparator<MetroAbstractEvent>() {
//		@Override
//		public int compare(MetroAbstractEvent o1, MetroAbstractEvent o2) {
//			int i;
//			i = (int) Math.signum(o1.offset - o2.offset);
//			if (i != 0 )
//				return i;
//			
//			return 0;
//		}
//	};
	
	public static final Comparator<? super MetroAbstractEvent> comparator = new Comparator<MetroAbstractEvent>() {
		@Override
		public int compare(MetroAbstractEvent o1, MetroAbstractEvent o2) {
			int i;
			i = (int) Math.signum(o1.barOffset - o2.barOffset);
			if (i != 0 )
				return i;
			
			if ( o1 instanceof MetroAbstractMidiEvent &&  o2 instanceof MetroAbstractMidiEvent ) {
				MetroAbstractMidiEvent mo1 = (MetroAbstractMidiEvent) o1;
				MetroAbstractMidiEvent mo2 = (MetroAbstractMidiEvent) o2;
				byte b1 = (byte) ( mo1.midiData[0] & 0b111100000 );
				byte b2 = (byte) ( mo2.midiData[0] & 0b111100000 );
				
				if ( b1 == b2 )
					return 0;
				
				if ( b1 == 0b10010000 )
					return 1;
				else 
					return -1;
						
			} else {
				return 0;
			}
		}
	};

	
	final double barOffset;
	int barOffsetInFrames;
	int midiOffset;
	
	public MetroAbstractEvent(double offset ) {
		super();
		this.barOffset = offset;
	}
	public final double getBarOffset() {
		return barOffset;
	}
	public final int getBarOffsetInFrames() {
		return barOffsetInFrames;
	}
	public final void calcInFrames( int barLengthInFrames ){
		this.barOffsetInFrames = (int)(this.barOffset * barLengthInFrames );
	}
	public final boolean between( int from, int to ) {
		return from <= this.barOffsetInFrames && this.barOffsetInFrames < to;
	}
	
	public final int getMidiOffset() {
		return midiOffset;
	}
	public final void setMidiOffset(int midiOffset) {
		this.midiOffset = midiOffset;
	}
	
	public abstract void process( Metro metro, int from, int to, int nframes, List<AbstractMidiEvent> eventList );
	
	public final String dump(String prefix) {
		StringBuilder sb = new StringBuilder();
		dumpProc(prefix, sb);
		return sb.toString();
	}
	public void dumpProc( String prefix, StringBuilder sb ) {
		sb.append(prefix).append( "            offset: " + barOffset ).append( "\n" );
		sb.append(prefix).append( "    offsetInFrames: " + barOffsetInFrames ).append( "\n" );
	}

//	public static void main(String[] args) {
//		MetroMidiEvent event = new MetroMidiEvent(1, 0.0d, new byte[] {} );
//		event.calcInFrames(48000);
//		boolean b = event.between(-1, 4);
//		System.out.println( b );
//	}

}
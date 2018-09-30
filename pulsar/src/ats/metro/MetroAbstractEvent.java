package ats.metro;

import java.util.Comparator;

public class MetroAbstractEvent {
	public static final Comparator<? super MetroAbstractEvent> comparator = new Comparator<MetroAbstractEvent>() {
		@Override
		public int compare(MetroAbstractEvent o1, MetroAbstractEvent o2) {
			int i;
			i = (int) Math.signum(o1.offset - o2.offset);
			if (i != 0 )
				return i;
			
			return 0;
		}
	};
	
	final double offset;
	int offsetInFrames;
	
	public MetroAbstractEvent(double offset ) {
		super();
		this.offset = offset;
	}
	public final double getOffset() {
		return offset;
	}
	public final int getOffsetInFrames() {
		return offsetInFrames;
	}
	public final void calcInFrames( int barInFrames ){
		this.offsetInFrames = (int)(this.offset * barInFrames );
	}
	public final boolean between(int from, int to ) {
		return from <= this.offsetInFrames && this.offsetInFrames < to;
	}

	public final String dump(String prefix) {
		StringBuilder sb = new StringBuilder();
		dumpProc(prefix, sb);
		return sb.toString();
	}
	public void dumpProc( String prefix, StringBuilder sb ) {
		sb.append(prefix).append( "            offset: " + offset ).append( "\n" );
		sb.append(prefix).append( "    offsetInFrames: " + offsetInFrames ).append( "\n" );
	}

//	public static void main(String[] args) {
//		MetroMidiEvent event = new MetroMidiEvent(1, 0.0d, new byte[] {} );
//		event.calcInFrames(48000);
//		boolean b = event.between(-1, 4);
//		System.out.println( b );
//	}

}
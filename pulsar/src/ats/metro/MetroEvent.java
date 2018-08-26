package ats.metro;

import java.util.Comparator;

public class MetroEvent {
	public static final Comparator<? super MetroEvent> comparator = new Comparator<MetroEvent>() {
		@Override
		public int compare(MetroEvent o1, MetroEvent o2) {
			return (int)(o1.offset - o2.offset);
		}
	};
	
	final double offset;
	int offsetInFrames;
	
	public MetroEvent(double offset ) {
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

	public final String dump(String string) {
		StringBuilder sb = new StringBuilder();
		dumpProc(string, sb);
		return sb.toString();
	}
	public void dumpProc( String prefix, StringBuilder sb ) {
		MetroEvent e = this;
		sb.append(prefix).append( "            offset: " + e.offset ).append( "\n" );
		sb.append(prefix).append( "    offsetInFrames: " + e.offsetInFrames ).append( "\n" );
	}

//	public static void main(String[] args) {
//		MetroMidiEvent event = new MetroMidiEvent(1, 0.0d, new byte[] {} );
//		event.calcInFrames(48000);
//		boolean b = event.between(-1, 4);
//		System.out.println( b );
//	}

}
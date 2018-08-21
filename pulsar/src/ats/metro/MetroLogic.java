package ats.metro;

import java.util.List;

public interface MetroLogic {
	public static double rnd(double from, double to) {
		return Math.random() * ( to - from ) + from;
	}
	public static double rnd(double d) {
		return rnd(-d,d);
	}
	
	public abstract void setLogicHandle( MetroLogicHandle handle );
	public abstract void processInputMidiBuffer( Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out );
	public abstract boolean processOutputNoteBuffer( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf );
	
	public static abstract class Default implements MetroLogic {
		protected MetroLogicHandle handle;
		@Override
		public void setLogicHandle(MetroLogicHandle handle) {
			this.handle = handle;
		}
	}
}

package ats.metro;

import java.util.List;

public interface MetroLogic {
	public static double rnd(double from, double to) {
		return Math.random() * ( to - from ) + from;
	}
	public static double rnd(double d) {
		return rnd(-d,d);
	}
	
	public abstract Metro getParent();
	public abstract void setParent( Metro metro );
	public abstract void setLogicHandle( MetroLogicHandle handle );
	public abstract boolean processOutputNoteBuffer( MetroNoteEventBuffer buf );
	public abstract void processInputMidiBuffer( List<MetroMidiEvent> in, List<MetroMidiEvent> out );
	
	public static abstract class Default implements MetroLogic {
		protected Metro parent;
		@Override
		public Metro getParent() {
			return this.parent;
		}
		@Override
		public void setParent(Metro parent) {
			this.parent = parent;
		}

		protected MetroLogicHandle handle;
		@Override
		public void setLogicHandle(MetroLogicHandle handle) {
			this.handle = handle;
		}
	}
}

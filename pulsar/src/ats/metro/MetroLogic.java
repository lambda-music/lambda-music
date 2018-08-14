package ats.metro;

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
	
	public abstract boolean processBuffer( MetroMidiEventBuffer buf );
	// public abstract boolean processInput( MetroMidiEventBuffer buf );
	
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

package nu.oka.metro;

public interface MetroLogic {
	public static double rnd(double from, double to) {
		return Math.random() * ( to - from ) + from;
	}
	public static double rnd(double d) {
		return rnd(-d,d);
	}
	void setLogicHandle( MetroLogicHandle handle );
	boolean processBuffer( MetroMidiEventBuffer buf );
	
	public static abstract class Default implements MetroLogic {
		protected MetroLogicHandle handle;
		@Override
		public void setLogicHandle(MetroLogicHandle handle) {
			this.handle = handle;
		}
	}
}

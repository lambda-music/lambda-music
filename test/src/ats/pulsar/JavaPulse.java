package ats.pulsar;

final class JavaPulse {
	static final int DEFAULT_PORT = 1;
	static final int DEFAULT_VELOCITY = 80;
	public final boolean centerPulse;
	public final int port;
	public final int channel;
	public final int note;
	public final int velocity;
	
	public JavaPulse(boolean centerPulse, int port, int channel, int note, int velocity) {
		super();
		this.centerPulse= centerPulse;
		this.port = port;
		this.channel = channel;
		this.note = note;
		this.velocity = velocity;
	}
	public JavaPulse(int port, int channel, int note, int velocity) {
		super();
		this.centerPulse= false;
		this.port = port;
		this.channel = channel;
		this.note = note;
		this.velocity = velocity;
	}
	public JavaPulse(int channel, int note, int velocity) {
		super();
		this.centerPulse= false;
		this.port = DEFAULT_PORT;
		this.channel = channel;
		this.note = note;
		this.velocity = velocity;
	}
	public JavaPulse(int note, int velocity ) {
		super();
		this.centerPulse= false;
		this.port = DEFAULT_PORT;
		this.channel = 1;
		this.note = note;
		this.velocity = velocity;
	}
	public JavaPulse(int note ) {
		super();
		this.centerPulse= false;
		this.port = DEFAULT_PORT;
		this.channel = 0;
		this.note = note;
		this.velocity = DEFAULT_VELOCITY;
	}
	public JavaPulse(boolean centerPulse, int channel, int note, int velocity) {
		super();
		this.centerPulse= centerPulse;
		this.port = DEFAULT_PORT;
		this.channel = channel;
		this.note = note;
		this.velocity = velocity;
	}
	public JavaPulse(boolean centerPulse, int note, int velocity ) {
		super();
		this.centerPulse= centerPulse;
		this.port = DEFAULT_PORT;
		this.channel = 1;
		this.note = note;
		this.velocity = velocity;
	}
	public JavaPulse(boolean centerPulse, int note ) {
		super();
		this.centerPulse= centerPulse;
		this.port = DEFAULT_PORT;
		this.channel = 0;
		this.note = note;
		this.velocity = DEFAULT_VELOCITY;
	}
}
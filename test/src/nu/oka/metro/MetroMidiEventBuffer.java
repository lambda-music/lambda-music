package nu.oka.metro;

import static nu.oka.metro.Metro.DEBUG;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackPosition;

public class MetroMidiEventBuffer implements Iterable<MetroMidiEvent>{
	private double humanizeFactorOffset=0;
	private double humanizeFactorVelocity=0;

	private double offset;
	private double length = 1.0d;
	private int lengthInFrames;
	private final List<MetroMidiEvent> list = new ArrayList<MetroMidiEvent>(10);
	public double getLength() {
		return length;
	}
	public void setLength(double length) {
		this.length = length;
	}
	public void setOffset(double offset) {
		this.offset = offset;
	}
	public double getOffset() {
		return offset;
	}
	public int getLengthInFrames() {
		return lengthInFrames;
	}
	
	public void prepare( JackClient client, JackPosition position ) throws JackException {
		int barInFrames = Metro.calcBarInFrames(client, position);
		this.calcInFrames( barInFrames );
	}
	
	private void calcInFrames( int barInFrames ) {
//		System.out.println("MetroMidiEventBuffer.calcInFrames() barInFrames="  + barInFrames );
		for ( MetroMidiEvent e : this ) {
			e.calcInFrames( barInFrames );
		}
		this.lengthInFrames = (int) (this.length * barInFrames);
		if ( DEBUG ) System.out.println("MetroMidiEventBuffer.calcInFrames() barInFrames="  + barInFrames + " / lengthInFrames=" + this.lengthInFrames  + "/ length=" + this.length  );
	}
	
	@Override
	public Iterator<MetroMidiEvent> iterator() {
		return this.list.iterator();
	}

	private void note(int outputPortNo, int midiEventValue, double offset, int channel, int note, int velocity) {
//		System.out.println( "note 1:" + Integer.toUnsignedString( midiEventValue , 2) );

		MetroMidiEvent event = new MetroMidiEvent(
				outputPortNo,
				offset,
				new byte[] {
						(byte)( ( 0b11110000 & midiEventValue ) | ( 0b00001111 & channel ) ),
						(byte) note,
						(byte) velocity
				}
		);
//		if ( this.length < offset ) {
//			this.length = offset;
//		}
//		System.out.println( "note 2:" + Integer.toUnsignedString(event.data[0] , 2) );
		this.list.add(event);
	}

	public void noteShot( double offset, int outputPortNo, int channel, int note, int velocity ) {
		noteOn( offset, outputPortNo, channel, note, velocity );
		noteOff( offset+0.0025d, outputPortNo, channel, note, velocity );
//		noteOff( offset+1.000d, outputPortNo, channel, note, velocity );
	}

	public void noteOn( double offset, int outputPortNo, int channel, int note, int velocity ) {
		if ( this.humanizeFactorOffset != 0.0d ) {
			offset += MetroLogic.rnd( this.humanizeFactorOffset );
//			if ( offset < 0 )  offset=0;
//			if ( 127 < offset  ) offset=127;
		}
		if ( this.humanizeFactorVelocity != 0.0d ) {
			velocity += MetroLogic.rnd( this.humanizeFactorVelocity ) ;
			if ( velocity < 0 )  velocity =0;
			if ( 127 < velocity ) velocity =127;
		}
		
		note( outputPortNo, 0b10010000, offset, channel, note, velocity );
	}
	public void noteOff( double offset, int outputPortNo, int channel, int note, int velocity ) {
		if ( this.humanizeFactorOffset != 0.0d )
			offset += MetroLogic.rnd( this.humanizeFactorOffset );
		if ( this.humanizeFactorVelocity != 0.0d ) {
			velocity += MetroLogic.rnd( this.humanizeFactorVelocity ) ;
			if ( velocity < 0 )  velocity =0;
			if ( 127 < velocity ) velocity =127;
		}

		note( outputPortNo, 0b10000000, offset, channel, note, velocity );
	}
	public void length( double length ) {
		this.length = length;
	}
	public void dump() {
		System.out.println( "length         : " + this.length );
		System.out.println( "lengthInFrames : " + this.lengthInFrames );
		int i = 0;
		for ( MetroMidiEvent e : this ) {
			System.out.println( "    No" + i );
			System.out.println( e.dump( "    " ) );
			i++;
		}
		System.out.println( "    END" );
	}
	public void humanize( double offset, double velocity ) {
		this.humanizeFactorOffset = offset;
		this.humanizeFactorVelocity = velocity;
	}
}

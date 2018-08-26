package ats.metro;

import static ats.metro.Metro.DEBUG;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackPosition;

import gnu.mapping.Environment;
import gnu.mapping.Procedure;

public class MetroNoteEventBuffer implements Iterable<MetroEvent>{
	private double humanizeFactorOffset=0;
	private double humanizeFactorVelocity=0;

	private double offset;
	private double length = 1.0d;
	private int lengthInFrames;
	private final List<MetroEvent> list = new ArrayList<MetroEvent>(10);
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
	
	public void prepare( Metro metro, JackClient client, JackPosition position ) throws JackException {
		this.list.sort( MetroNoteEvent.comparator );
		int barInFrames = Metro.calcBarInFrames( metro, client, position );
		this.calcInFrames( barInFrames );
	}
	
	private void calcInFrames( int barInFrames ) {
//		System.out.println("MetroMidiEventBuffer.calcInFrames() barInFrames="  + barInFrames );
		for ( MetroEvent e : this ) {
			e.calcInFrames( barInFrames );
		}
		this.lengthInFrames = (int) (this.length * barInFrames);
		if ( DEBUG ) System.out.println("MetroMidiEventBuffer.calcInFrames() barInFrames="  + barInFrames + " / lengthInFrames=" + this.lengthInFrames  + "/ length=" + this.length  );
	}
	
	@Override
	public Iterator<MetroEvent> iterator() {
		return this.list.iterator();
	}

	private void note(int outputPortNo, int midiEventValue, double offset, int channel, int note, int velocity) {
		MetroNoteEvent event = new MetroNoteEvent(
				offset,
				outputPortNo,
				new byte[] {
						(byte)( ( 0b11110000 & midiEventValue ) | ( 0b00001111 & channel ) ),
						(byte) note,
						(byte) velocity
				}
		);
		this.list.add(event);
	}

	public void noteHit( double offset, int outputPortNo, int channel, int note, int velocity ) {
		noteHit( offset, outputPortNo, channel, note, velocity, -1 );
	}
	public void noteHit( double offset, int outputPortNo, int channel, int note, int velocity, double duration ) {
		if ( 0 < duration )
			duration = 0.0025d;
		noteOn(  offset, outputPortNo, channel, note, velocity );
		noteOff( offset + duration, outputPortNo, channel, note, velocity );
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
	public void exec( double offset, Environment environment, Procedure procedure ) {
		MetroSchemeProcedureEvent event = 
				new MetroSchemeProcedureEvent( 
						offset,
						environment,
						procedure );

		this.list.add( event );
	}

	public void length( double length ) {
		this.length = length;
	}
	public void dump() {
		System.out.println( "length         : " + this.length );
		System.out.println( "lengthInFrames : " + this.lengthInFrames );
		int i = 0;
		for ( MetroEvent e : this ) {
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

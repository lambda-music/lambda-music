/*
 * Metro Musical Sequencing Framework written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Metro Musical Sequencing Framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Metro Musical Sequencing Framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Metro Musical Sequencing Framework.  If not, see <https://www.gnu.org/licenses/>.
 */

package ats.metro;

import static ats.metro.Metro.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackPosition;

/**
 * This class represents all events which should be processed in a bar. 
 * 
 * @author Ats Oka
 */
public class MetroEventBuffer implements Iterable<MetroEvent>, MetroBufferedMidiReceiver {
	static final Logger LOGGER = Logger.getLogger(MetroEventBuffer.class.getName());
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		// LOGGER.log(Level.INFO, msg);
		System.err.println(msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}

    private static final MetroMidiMessage MIDI_MESSAGE_GEN = MetroMidiMessage.getInstance();
	
	private double humanizeOffset_min = 0;
	private double humanizeOffset_max = 0;
	private double humanizeOffset_size = 0;
	private double humanizeVelocity_min = 0;
	private double humanizeVelocity_max = 0;
	private double humanizeVelocity_size = 0;

	public void setHumanizeOffset( double min, double max ) {
		this.humanizeOffset_min = min;
		this.humanizeOffset_max = max;
		humanizeOffset_size = this.humanizeOffset_max - this.humanizeOffset_min;
	}
	public void setHumanizeVelocity( double min, double max ) {
		this.humanizeVelocity_min = min;
		this.humanizeVelocity_max = max;
		this.humanizeVelocity_size = this.humanizeVelocity_max - this.humanizeVelocity_min;
	}

	
	/*
	 * NOT USED
	 */
	@Deprecated
	private double offset;
	private double length = 1.0d;
	private boolean prepared = false;
	private int barLengthInFrames=-1;
	private int lengthInFrames = -1;
	private final List<MetroEvent> list = new ArrayList<MetroEvent>(10);
	public double getLength() {
		return length;
	}
	public void setLength(double length) {
		if (DEBUG) logInfo( "setLength():" + length );
		this.length = length;
	}
	public double getActualLength() {
		double max = 0;
		for ( MetroEvent e : this )
			if ( max < e.barOffset ) 
				max = e.barOffset;
		
		return max;
	}
	/*
	 * NOT USED
	 */
	@Deprecated
	public void setOffset(double offset) {
		this.offset = offset;
	}
	/*
	 * NOT USED
	 */
	@Deprecated
	public double getOffset() {
		return offset;
	}
	public int getBarLengthInFrames() {
		if ( ! prepared )
			throw new RuntimeException("not prepared");
		return barLengthInFrames;
	}
	public int getLengthInFrames() {
		if ( ! prepared )
			throw new RuntimeException("not prepared");
		return lengthInFrames;
	}
	
	public void prepare( Metro metro, JackClient client, JackPosition position, boolean doSort ) throws JackException {
		if ( doSort )
			this.list.sort( MetroMidiEvent.comparator );
		int barInFrames = Metro.calcBarInFrames( metro, client, position );
		this.calcInFrames( barInFrames );
	}
	
	private void calcInFrames( int barLengthInFrames ) {
//		System.out.println("MetroMidiEventBuffer.calcInFrames() barInFrames="  + barInFrames );
		for ( MetroEvent e : this ) {
			e.calcInFrames( barLengthInFrames );
		}
//		System.out.println( "this.length " + this.length  );
		this.barLengthInFrames = barLengthInFrames;
		this.lengthInFrames = (int) (this.length * (double)barLengthInFrames);
		this.prepared = true;
		
		if ( DEBUG ) 
			logInfo( "MetroMidiEventBuffer.calcInFrames() barInFrames="  + barLengthInFrames + " / lengthInFrames=" + this.lengthInFrames  + "/ length=" + this.length);
	}
	
	@Override
	public Iterator<MetroEvent> iterator() {
		return this.list.iterator();
	}
	
	public int size() {
		return this.list.size();
	}

	public final void event( MetroEvent event ) {
		// Add it to the list.
		this.list.add(event);
	}
	
	public final void midiEvent( double offset, int outputPortNo, byte[] data ) {
		// Create an event object.
		MetroMidiEvent event = new MetroMidiEvent( offset, outputPortNo, data );
		
		// Add it to the list.
		this.list.add(event);
	}
	
	private void note(int outputPortNo, int midiEventValue, double offset, int channel, int note, double velocity) {
		/*
		 * DON'T CHECK MIN/MAX HERE
		 * The offset may go beyond the minimum/maximum; now 
		 * {@link MetroEventBuffer} can process the MIDI 
		 * signals which are beyond the region of the buffer.   
		 */
		// if ( offset < 0 )  offset=0;
		// if ( 127 < offset  ) offset=127;

		if ( velocity < 0 )  velocity =0d;
		if ( 1d < velocity ) velocity =1d;

		// Create an event object.
		MetroMidiEvent event = new MetroMidiEvent(
				offset,
				outputPortNo,
				new byte[] {
						(byte)( ( 0b11110000 & midiEventValue ) | ( 0b00001111 & channel ) ),
						(byte) note,
						(byte) (127d * velocity)
				}
		);
		
		// Add it to the list.
		this.list.add(event);
	}

	public void noteHit( double offset, int outputPortNo, int channel, int note, double velocity ) {
		noteHit( offset, outputPortNo, channel, note, velocity, -1 );
	}
	public void noteHit( double offset, int outputPortNo, int channel, int note, double velocity, double duration ) {
		if ( duration < 0 )
			duration = 0.0025d;
		
		noteOn(  offset,            outputPortNo, channel, note, velocity );
		noteOff( offset + duration, outputPortNo, channel, note, velocity );
	}

	MetroNoteInfoMap noteInfoMap = new MetroNoteInfoMap();
	public void noteOnBak( double offset, int outputPortNo, int channel, int note, double velocity ) {
		double humanizeOffset =  this.humanizeVelocity_min;
		double humanizeVelocity =  this.humanizeVelocity_min;
		if ( humanizeOffset_size != 0.0d ) {
			humanizeOffset =+ ( Math.random() * this.humanizeOffset_size );
		}
		if ( this.humanizeVelocity_size != 0.0d ) {
			humanizeVelocity =+ ( Math.random() *  this.humanizeVelocity_size );
		}
		
		offset   += humanizeOffset;
		velocity += humanizeVelocity;

		noteInfoMap.put(outputPortNo, channel, note, humanizeOffset, humanizeVelocity);

		note( outputPortNo, 0b10010000, offset, channel, note, velocity );
	}

	public void noteOffBak( double offset, int outputPortNo, int channel, int note, double velocity ) {
		MetroNoteInfoMap.Value value = noteInfoMap.get(outputPortNo, channel, note );

		note( outputPortNo, 0b10000000, offset + value.offset, channel, note, velocity + value.velocity );
	}
	
	
	public void exec( double offset, Runnable runnable ) {
		MetroMessageEvent event = new MetroMessageEvent( offset, runnable );

		this.list.add( event );
	}

//	public void length( double length ) {
//		this.length = length;
//	}
	public void dump() {
		logInfo( "length         : " + this.length        );
		logInfo( "lengthInFrames : " + this.lengthInFrames);
		int i = 0;
		for ( MetroEvent e : this ) {
			logInfo( "    No" + i);
			logInfo( e.dump( "    " ));
			i++;
		}
		logInfo( "    END");
	}
	
	//

	// basic 
	public void noteOn( double offset, int port, int channel, int note, double velocity) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.noteOn(channel, note, velocity));
	}
	public void noteOn( double offset, int port, int channel, int note, int velocity) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.noteOn(channel, note, velocity));
	}
	public void noteOff( double offset, int port, int channel, int note, double velocity) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.noteOff(channel, note, velocity));
	}
	public void noteOff( double offset, int port, int channel, int note, int velocity) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.noteOff(channel, note, velocity));
	}
	public void keyPressure( double offset, int port, int channel, int note, double value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.keyPressure(channel, note, value));
	}
	public void keyPressure( double offset, int port, int channel, int note, int value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.keyPressure(channel, note, value));
	}
	public void controlChange( double offset, int port, int channel, int controlNumber, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.controlChange(channel, controlNumber, controlValue));
	}
	public void programChange( double offset, int port, int channel, int value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.programChange(channel, value));
	}
	public void channelPressure( double offset, int port, int channel, double value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.channelPressure(channel, value));
	}
	public void channelPressure( double offset, int port, int channel, int value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.channelPressure(channel, value));
	}
	public void pitchBend( double offset, int port, int channel, double value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.pitchBend(channel, value));
	}
	public void pitchBend( double offset, int port, int channel, int value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.pitchBend(channel, value));
	}
	public void cc_allSoundOff( double offset, int port, int channel) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_allSoundOff(channel));
	}
	public void cc_resetAllControllers( double offset, int port, int channel) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_resetAllControllers(channel));
	}
	public void cc_localControls( double offset, int port, int channel, boolean value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_localControls(channel, value));
	}
	public void cc_allNoteOff( double offset, int port, int channel) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_allNoteOff(channel));
	}
	public void cc_omniModeOff( double offset, int port, int channel) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_omniModeOff(channel));
	}
	public void cc_omniModeOn( double offset, int port, int channel) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_omniModeOn(channel));
	}
	public void cc_monoModeOn( double offset, int port, int channel) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_monoModeOn(channel));
	}
	public void cc_polyModeOn( double offset, int port, int channel) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_polyModeOn(channel));
	}
	public void songPositionPointer( double offset, int port, int value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.songPositionPointer(value));
	}
	public void songSelect( double offset, int port, int value) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.songSelect(value));
	}
	public void endOfExclusive(  double offset, int port ) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.endOfExclusive());
	}
	public void clock(  double offset, int port ) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.clock());
	}
	public void start(  double offset, int port ){
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.start());
	}
	public void cont(  double offset, int port ) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cont());
	}
	public void stop(  double offset, int port ) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.stop());
	}
	public void reset(  double offset, int port ) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.reset());
	}
	public void cc_bankSelect( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_bankSelect(channel, controlValue));
	}
	public void cc_modulation( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_modulation(channel, controlValue));
	}
	public void cc_breathController( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_breathController(channel, controlValue));
	}
	public void cc_footController( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_footController(channel, controlValue));
	}
	public void cc_portamentoTime( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_portamentoTime(channel, controlValue));
	}
	public void cc_dataEntryMsb( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_dataEntryMsb(channel, controlValue));
	}
	public void cc_volume( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_volume(channel, controlValue));
	}
	public void cc_balance( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_balance(channel, controlValue));
	}
	public void cc_pan( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_pan(channel, controlValue));
	}
	public void cc_expression( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_expression(channel, controlValue));
	}
	public void cc_effectController1( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_effectController1(channel, controlValue));
	}
	public void cc_effectController2( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_effectController2(channel, controlValue));
	}
	public void cc_sustainPedal( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_sustainPedal(channel, controlValue));
	}
	public void cc_portamentoSwitch( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_portamentoSwitch(channel, controlValue));
	}
	public void cc_sostenutoSwitch( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_sostenutoSwitch(channel, controlValue));
	}
	public void cc_pedalSwitch( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_pedalSwitch(channel, controlValue));
	}
	public void cc_legatoSwitch( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_legatoSwitch(channel, controlValue));
	}
	public void cc_hold2( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_hold2(channel, controlValue));
	}
	public void cc_soundController1( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController1(channel, controlValue));
	}
	public void cc_soundController2( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController2(channel, controlValue));
	}
	public void cc_soundController3( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController3(channel, controlValue));
	}
	public void cc_soundController4( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController4(channel, controlValue));
	}
	public void cc_soundController5( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController5(channel, controlValue));
	}
	public void cc_soundController6( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController6(channel, controlValue));
	}
	public void cc_soundController7( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController7(channel, controlValue));
	}
	public void cc_soundController8( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController8(channel, controlValue));
	}
	public void cc_soundController9( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController9(channel, controlValue));
	}
	public void cc_soundController10( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_soundController10(channel, controlValue));
	}
	public void cc_generalPurpose01( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose01(channel, controlValue));
	}
	public void cc_generalPurpose02( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose02(channel, controlValue));
	}
	public void cc_generalPurpose03( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose03(channel, controlValue));
	}
	public void cc_generalPurpose04( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose04(channel, controlValue));
	}
	public void cc_portamento( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_portamento(channel, controlValue));
	}
	public void cc_effect1( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_effect1(channel, controlValue));
	}
	public void cc_effect2( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_effect2(channel, controlValue));
	}
	public void cc_effect3( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_effect3(channel, controlValue));
	}
	public void cc_effect4( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_effect4(channel, controlValue));
	}
	public void cc_effect5( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_effect5(channel, controlValue));
	}
	public void cc_dataIncrement( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_dataIncrement(channel, controlValue));
	}
	public void cc_dataDecrement( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_dataDecrement(channel, controlValue));
	}
	public void cc_nrpnLsb( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_nrpnLsb(channel, controlValue));
	}
	public void cc_nrpnMsb( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_nrpnMsb(channel, controlValue));
	}
	public void cc_rpnLsb( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_rpnLsb(channel, controlValue));
	}
	public void cc_rpnMsb( double offset, int port, int channel, int controlValue) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.cc_rpnMsb(channel, controlValue));
	}
	public void error( double offset, int port, String string) {
		this.midiEvent( offset, port, MIDI_MESSAGE_GEN.error(string));
	}
}


class MetroNoteInfoMap {
	static final Value ZERO_VALUE = new Value(0, 0);
	public static class Key {
		final int port;
		final int channel;
		final int note;
		public Key(int port, int channel, int note) {
			super();
			this.port = port;
			this.channel = channel;
			this.note = note;
		}
		@Override
		public int hashCode() {
			return ( 1 * channel * port * 256 + note * 65536 ) ;
		}
		@Override
		public boolean equals(Object obj) {
			if ( obj instanceof Key ) {
				Key k = (Key)obj;
				return 
						( k.port == this.port ) &&
						( k.channel == this.channel ) &&
						( k.note == this.note );
			} else {
				return false;
			}
		}
	}
	public static class Value {
		final double offset;
		final double velocity;
		public Value(double offset, double velocity) {
			super();
			this.offset = offset;
			this.velocity = velocity;
		}
	}
	
	final HashMap<Key,Value> map = new HashMap<>();
	public void put( int port, int channel, int note , double offset, double velocity ) {
		map.put( new Key(port, channel, note), new Value(offset, velocity) );
	}
	public boolean containsKey( int port, int channel, int note ) {
		return map.containsKey( new Key(port, channel, note) );
	}
	public MetroNoteInfoMap.Value get( int port, int channel, int note ) {
		Value value = map.get( new Key(port, channel, note) );
		return value == null  ? ZERO_VALUE : value;
	}
	public void clear() {
		this.map.clear();
	}
}



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

package metro;

import static metro.Metro.*;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
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
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
	static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
	static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    private static final MetroMidiMessage MIDI_MESSAGE_GEN = MetroMidiMessage.getInstance();
	
	private double length = 1.0d;
	private boolean prepared = false;
	private int barLengthInFrames=-1;
	private int lengthInFrames = -1;
	private final List<MetroEvent> list = new ArrayList<MetroEvent>(10);
	public double getLength() {
		return length;
	}
	public void setLength(double length) {
		if (DEBUG) 
			logInfo( "setLength():" + length );
		this.length = length;
	}
	public double getActualLength() {
		double max = 0;
		for ( MetroEvent e : this )
			if ( max < e.barOffset ) 
				max = e.barOffset;
		
		return max;
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
	
	public final void midiEvent( String id, double offset, MetroPort outputPort, byte[] data ) {
		// Create an event object.
		MetroMidiEvent event = new MetroMidiEvent( id, offset, outputPort, data );
		
		// Add it to the list.
		this.list.add(event);
	}
	
	public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity ) {
		noteHit( offset, outputPort, channel, note, velocity, -1 );
	}
	public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity, double duration ) {
		if ( duration < 0 )
			duration = 0.0025d;
		
		noteOn(  offset,            outputPort, channel, note, velocity );
		noteOff( offset + duration, outputPort, channel, note, velocity );
	}

	public void exec( double offset, Runnable runnable ) {
		MetroMessageEvent event = new MetroMessageEvent( "exec", offset, runnable );

		this.list.add( event );
	}

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
	public void noteOn( double offset, MetroPort port, int channel, int note, double velocity) {
		this.midiEvent( "noteOn", offset, port, MIDI_MESSAGE_GEN.noteOn(channel, note, velocity));
	}
	public void noteOn( double offset, MetroPort port, int channel, int note, int velocity) {
		this.midiEvent( "noteOn", offset, port, MIDI_MESSAGE_GEN.noteOn(channel, note, velocity));
	}
	public void noteOff( double offset, MetroPort port, int channel, int note, double velocity) {
		this.midiEvent( "noteOff", offset, port, MIDI_MESSAGE_GEN.noteOff(channel, note, velocity));
	}
	public void noteOff( double offset, MetroPort port, int channel, int note, int velocity) {
		this.midiEvent( "noteOff", offset, port, MIDI_MESSAGE_GEN.noteOff(channel, note, velocity));
	}
	public void keyPressure( double offset, MetroPort port, int channel, int note, double value) {
		this.midiEvent( "keyPressure", offset, port, MIDI_MESSAGE_GEN.keyPressure(channel, note, value));
	}
	public void keyPressure( double offset, MetroPort port, int channel, int note, int value) {
		this.midiEvent( "keyPressure", offset, port, MIDI_MESSAGE_GEN.keyPressure(channel, note, value));
	}
	public void controlChange( double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
		this.midiEvent( "controlChange", offset, port, MIDI_MESSAGE_GEN.controlChange(channel, controlNumber, controlValue));
	}
	public void programChange( double offset, MetroPort port, int channel, int value) {
		this.midiEvent( "programChange", offset, port, MIDI_MESSAGE_GEN.programChange(channel, value));
	}
	public void channelPressure( double offset, MetroPort port, int channel, double value) {
		this.midiEvent( "channelPressure", offset, port, MIDI_MESSAGE_GEN.channelPressure(channel, value));
	}
	public void channelPressure( double offset, MetroPort port, int channel, int value) {
		this.midiEvent( "channelPressure", offset, port, MIDI_MESSAGE_GEN.channelPressure(channel, value));
	}
	public void pitchBend( double offset, MetroPort port, int channel, double value) {
		this.midiEvent( "pitchBend", offset, port, MIDI_MESSAGE_GEN.pitchBend(channel, value));
	}
	public void pitchBend( double offset, MetroPort port, int channel, int value) {
		this.midiEvent( "pitchBend", offset, port, MIDI_MESSAGE_GEN.pitchBend(channel, value));
	}
	public void cc_allSoundOff( double offset, MetroPort port, int channel) {
		this.midiEvent( "cc_allSoundOff", offset, port, MIDI_MESSAGE_GEN.cc_allSoundOff(channel));
	}
	public void cc_resetAllControllers( double offset, MetroPort port, int channel) {
		this.midiEvent( "cc_resetAllControllers", offset, port, MIDI_MESSAGE_GEN.cc_resetAllControllers(channel));
	}
	public void cc_localControls( double offset, MetroPort port, int channel, boolean value) {
		this.midiEvent( "cc_localControls", offset, port, MIDI_MESSAGE_GEN.cc_localControls(channel, value));
	}
	public void cc_allNoteOff( double offset, MetroPort port, int channel) {
		this.midiEvent( "cc_allNoteOff", offset, port, MIDI_MESSAGE_GEN.cc_allNoteOff(channel));
	}
	public void cc_omniModeOff( double offset, MetroPort port, int channel) {
		this.midiEvent( "cc_omniModeOff", offset, port, MIDI_MESSAGE_GEN.cc_omniModeOff(channel));
	}
	public void cc_omniModeOn( double offset, MetroPort port, int channel) {
		this.midiEvent( "cc_omniModeOn", offset, port, MIDI_MESSAGE_GEN.cc_omniModeOn(channel));
	}
	public void cc_monoModeOn( double offset, MetroPort port, int channel) {
		this.midiEvent( "cc_monoModeOn", offset, port, MIDI_MESSAGE_GEN.cc_monoModeOn(channel));
	}
	public void cc_polyModeOn( double offset, MetroPort port, int channel) {
		this.midiEvent( "cc_polyModeOn", offset, port, MIDI_MESSAGE_GEN.cc_polyModeOn(channel));
	}
	public void songPositionPointer( double offset, MetroPort port, int value) {
		this.midiEvent( "songPositionPointer", offset, port, MIDI_MESSAGE_GEN.songPositionPointer(value));
	}
	public void songSelect( double offset, MetroPort port, int value) {
		this.midiEvent( "songSelect", offset, port, MIDI_MESSAGE_GEN.songSelect(value));
	}
	public void endOfExclusive(  double offset, MetroPort port ) {
		this.midiEvent( "endOfExclusive", offset, port, MIDI_MESSAGE_GEN.endOfExclusive());
	}
	public void clock(  double offset, MetroPort port ) {
		this.midiEvent( "clock", offset, port, MIDI_MESSAGE_GEN.clock());
	}
	public void start(  double offset, MetroPort port ){
		this.midiEvent( "start", offset, port, MIDI_MESSAGE_GEN.start());
	}
	public void cont(  double offset, MetroPort port ) {
		this.midiEvent( "cont", offset, port, MIDI_MESSAGE_GEN.cont());
	}
	public void stop(  double offset, MetroPort port ) {
		this.midiEvent( "stop", offset, port, MIDI_MESSAGE_GEN.stop());
	}
	public void reset(  double offset, MetroPort port ) {
		this.midiEvent( "reset", offset, port, MIDI_MESSAGE_GEN.reset());
	}
	public void cc_bankSelect( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_bankSelect", offset, port, MIDI_MESSAGE_GEN.cc_bankSelect(channel, controlValue));
	}
	public void cc_modulation( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_modulation", offset, port, MIDI_MESSAGE_GEN.cc_modulation(channel, controlValue));
	}
	public void cc_breathController( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_breathController", offset, port, MIDI_MESSAGE_GEN.cc_breathController(channel, controlValue));
	}
	public void cc_footController( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_footController", offset, port, MIDI_MESSAGE_GEN.cc_footController(channel, controlValue));
	}
	public void cc_portamentoTime( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_portamentoTime", offset, port, MIDI_MESSAGE_GEN.cc_portamentoTime(channel, controlValue));
	}
	public void cc_dataEntryMsb( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_dataEntryMsb", offset, port, MIDI_MESSAGE_GEN.cc_dataEntryMsb(channel, controlValue));
	}
	public void cc_volume( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_volume", offset, port, MIDI_MESSAGE_GEN.cc_volume(channel, controlValue));
	}
	public void cc_balance( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_balance", offset, port, MIDI_MESSAGE_GEN.cc_balance(channel, controlValue));
	}
	public void cc_pan( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_pan", offset, port, MIDI_MESSAGE_GEN.cc_pan(channel, controlValue));
	}
	public void cc_expression( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_expression", offset, port, MIDI_MESSAGE_GEN.cc_expression(channel, controlValue));
	}
	public void cc_effectController1( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_effectController1", offset, port, MIDI_MESSAGE_GEN.cc_effectController1(channel, controlValue));
	}
	public void cc_effectController2( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_effectController2", offset, port, MIDI_MESSAGE_GEN.cc_effectController2(channel, controlValue));
	}
	public void cc_sustainPedal( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_sustainPedal", offset, port, MIDI_MESSAGE_GEN.cc_sustainPedal(channel, controlValue));
	}
	public void cc_portamentoSwitch( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_portamentoSwitch", offset, port, MIDI_MESSAGE_GEN.cc_portamentoSwitch(channel, controlValue));
	}
	public void cc_sostenutoSwitch( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_sostenutoSwitch", offset, port, MIDI_MESSAGE_GEN.cc_sostenutoSwitch(channel, controlValue));
	}
	public void cc_pedalSwitch( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_pedalSwitch", offset, port, MIDI_MESSAGE_GEN.cc_pedalSwitch(channel, controlValue));
	}
	public void cc_legatoSwitch( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_legatoSwitch", offset, port, MIDI_MESSAGE_GEN.cc_legatoSwitch(channel, controlValue));
	}
	public void cc_hold2( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_hold2", offset, port, MIDI_MESSAGE_GEN.cc_hold2(channel, controlValue));
	}
	public void cc_soundController1( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController1", offset, port, MIDI_MESSAGE_GEN.cc_soundController1(channel, controlValue));
	}
	public void cc_soundController2( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController2", offset, port, MIDI_MESSAGE_GEN.cc_soundController2(channel, controlValue));
	}
	public void cc_soundController3( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController3", offset, port, MIDI_MESSAGE_GEN.cc_soundController3(channel, controlValue));
	}
	public void cc_soundController4( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController4", offset, port, MIDI_MESSAGE_GEN.cc_soundController4(channel, controlValue));
	}
	public void cc_soundController5( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController5", offset, port, MIDI_MESSAGE_GEN.cc_soundController5(channel, controlValue));
	}
	public void cc_soundController6( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController6", offset, port, MIDI_MESSAGE_GEN.cc_soundController6(channel, controlValue));
	}
	public void cc_soundController7( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController7", offset, port, MIDI_MESSAGE_GEN.cc_soundController7(channel, controlValue));
	}
	public void cc_soundController8( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController8", offset, port, MIDI_MESSAGE_GEN.cc_soundController8(channel, controlValue));
	}
	public void cc_soundController9( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController9", offset, port, MIDI_MESSAGE_GEN.cc_soundController9(channel, controlValue));
	}
	public void cc_soundController10( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_soundController10", offset, port, MIDI_MESSAGE_GEN.cc_soundController10(channel, controlValue));
	}
	public void cc_generalPurpose01( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_generalPurpose01", offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose01(channel, controlValue));
	}
	public void cc_generalPurpose02( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_generalPurpose02", offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose02(channel, controlValue));
	}
	public void cc_generalPurpose03( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_generalPurpose03", offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose03(channel, controlValue));
	}
	public void cc_generalPurpose04( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_generalPurpose04", offset, port, MIDI_MESSAGE_GEN.cc_generalPurpose04(channel, controlValue));
	}
	public void cc_portamento( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_portamento", offset, port, MIDI_MESSAGE_GEN.cc_portamento(channel, controlValue));
	}
	public void cc_effect1( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_effect1", offset, port, MIDI_MESSAGE_GEN.cc_effect1(channel, controlValue));
	}
	public void cc_effect2( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_effect2", offset, port, MIDI_MESSAGE_GEN.cc_effect2(channel, controlValue));
	}
	public void cc_effect3( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_effect3", offset, port, MIDI_MESSAGE_GEN.cc_effect3(channel, controlValue));
	}
	public void cc_effect4( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_effect4", offset, port, MIDI_MESSAGE_GEN.cc_effect4(channel, controlValue));
	}
	public void cc_effect5( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_effect5", offset, port, MIDI_MESSAGE_GEN.cc_effect5(channel, controlValue));
	}
	public void cc_dataIncrement( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_dataIncrement", offset, port, MIDI_MESSAGE_GEN.cc_dataIncrement(channel, controlValue));
	}
	public void cc_dataDecrement( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_dataDecrement", offset, port, MIDI_MESSAGE_GEN.cc_dataDecrement(channel, controlValue));
	}
	public void cc_nrpnLsb( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_nrpnLsb", offset, port, MIDI_MESSAGE_GEN.cc_nrpnLsb(channel, controlValue));
	}
	public void cc_nrpnMsb( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_nrpnMsb", offset, port, MIDI_MESSAGE_GEN.cc_nrpnMsb(channel, controlValue));
	}
	public void cc_rpnLsb( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_rpnLsb", offset, port, MIDI_MESSAGE_GEN.cc_rpnLsb(channel, controlValue));
	}
	public void cc_rpnMsb( double offset, MetroPort port, int channel, int controlValue) {
		this.midiEvent( "cc_rpnMsb", offset, port, MIDI_MESSAGE_GEN.cc_rpnMsb(channel, controlValue));
	}
	public void error( double offset, MetroPort port, String string) {
		this.midiEvent( "error", offset, port, MIDI_MESSAGE_GEN.error(string));
	}
}





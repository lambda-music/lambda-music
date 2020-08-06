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

/**
 * See {@link MetroMidiReceiver}.
 * 
 * There are two type of Midi receivers: {@link MetroMidiReceiver} and this {@link MetroBufferedMidiReceiver}. 
 * Both two classes are often intercahngablly converted to one another. See {@link MetroBufferedToDirectMidiReceiver} and
 * {@link MetroDirectToBufferedMidiReceiver}.
 *
 * @author Ats Oka
 */
public interface MetroBufferedMidiReceiver<T> {
    // system
    public boolean endCalled();
    public T end();
    public T error( double offset, MetroPort port, String message );

    // special 
    public T exec(  double offset, Runnable runnable );
    public T exet( double offset, MetroTrackManipulator trackManipulator );
    public T event( double offset, T event );
    public T length( double length );
    
    // basic 
    public T noteOn( double offset, MetroPort port, int channel, int note, double velocity );
    public T noteOn( double offset, MetroPort port, int channel, int note, int velocity );
    public T noteOff( double offset, MetroPort port, int channel, int note, double velocity);
    public T noteOff( double offset, MetroPort port, int channel, int note, int velocity );
    public T keyPressure( double offset, MetroPort port, int channel, int note, double pressure);
    public T keyPressure( double offset, MetroPort port, int channel, int note, int pressure);
    public T controlChange( double offset, MetroPort port, int channel, int controlNumber, int controlValue );
    public T programChange(  double offset, MetroPort port, int channel, int programNumber );
    public T channelPressure(  double offset, MetroPort port, int channel, double pressureValue );
    public T channelPressure(  double offset, MetroPort port, int channel, int pressureValue );
    public T pitchBend(  double offset, MetroPort port, int channel, double pitchBendValue );
    public T pitchBend(  double offset, MetroPort port, int channel, int pitchBendValue );

    public T cc_allSoundOff(  double offset, MetroPort port, int channel ); 
    public T cc_resetAllControllers(  double offset, MetroPort port, int channel ); 
    public T cc_localControls(  double offset, MetroPort port, int channel, boolean on ); 
    public T cc_allNoteOff(  double offset, MetroPort port, int channel ); 
    public T cc_omniModeOff(  double offset, MetroPort port, int channel ); 
    public T cc_omniModeOn(  double offset, MetroPort port, int channel ); 
    public T cc_monoModeOn(  double offset, MetroPort port, int channel ); 
    public T cc_polyModeOn(  double offset, MetroPort port, int channel ); 

    // system
    public T songPositionPointer( double offset, MetroPort port, int pos ); 
    public T songSelect( double offset, MetroPort port, int songNumber ); 
    public T endOfExclusive( double offset, MetroPort port ); 
    public T clock( double offset, MetroPort port ); 
    public T start( double offset, MetroPort port ); 
    public T cont( double offset, MetroPort port ); 
    public T stop( double offset, MetroPort port ); 
    public T reset( double offset, MetroPort port );

    // control change
    public T cc_bankSelect( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_modulation( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_breathController( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_footController( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_portamentoTime( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_dataEntryMsb( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_volume( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_balance( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_pan( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_expression( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_effectController1( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_effectController2( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_sustainPedal( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_portamentoSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_sostenutoSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_pedalSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_legatoSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_hold2( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController1( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController2( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController3( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController4( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController5( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController6( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController7( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController8( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController9( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_soundController10( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_generalPurpose01( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_generalPurpose02( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_generalPurpose03( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_generalPurpose04( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_portamento( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_effect1( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_effect2( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_effect3( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_effect4( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_effect5( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_dataIncrement( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_dataDecrement( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_nrpnLsb( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_nrpnMsb( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_rpnLsb( double offset, MetroPort port,  int channel, int value ) ;
    public T cc_rpnMsb( double offset, MetroPort port,  int channel, int value ) ;
    
    public static class Default<T> implements MetroBufferedMidiReceiver<T> {
        private final MetroBufferedMidiReceiver<T> receiver;
        protected T receive( T object ) {
            return object;
        }
        public boolean endCalled() {
            return receiver.endCalled();
        }
        public T end() {
            return receive( receiver.end());
        }
        public T error(double offset, MetroPort port, String message) {
            return receive( receiver.error(offset, port, message));
        }
        public T exec(double offset, Runnable runnable) {
            return receive( receiver.exec(offset, runnable));
        }
        public T exet(double offset, MetroTrackManipulator trackManipulator) {
            return receive( receiver.exet(offset, trackManipulator));
        }
        public T event(double offset, T event) {
            return receive( receiver.event(offset, event));
        }
        public T length(double length) {
            return receive( receiver.length(length));
        }
        public T noteOn(double offset, MetroPort port, int channel, int note, double velocity) {
            return receive( receiver.noteOn(offset, port, channel, note, velocity));
        }
        public T noteOn(double offset, MetroPort port, int channel, int note, int velocity) {
            return receive( receiver.noteOn(offset, port, channel, note, velocity));
        }
        public T noteOff(double offset, MetroPort port, int channel, int note, double velocity) {
            return receive( receiver.noteOff(offset, port, channel, note, velocity));
        }
        public T noteOff(double offset, MetroPort port, int channel, int note, int velocity) {
            return receive( receiver.noteOff(offset, port, channel, note, velocity));
        }
        public T keyPressure(double offset, MetroPort port, int channel, int note, double pressure) {
            return receive( receiver.keyPressure(offset, port, channel, note, pressure));
        }
        public T keyPressure(double offset, MetroPort port, int channel, int note, int pressure) {
            return receive( receiver.keyPressure(offset, port, channel, note, pressure));
        }
        public T controlChange(double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
            return receive( receiver.controlChange(offset, port, channel, controlNumber, controlValue));
        }
        public T programChange(double offset, MetroPort port, int channel, int programNumber) {
            return receive( receiver.programChange(offset, port, channel, programNumber));
        }
        public T channelPressure(double offset, MetroPort port, int channel, double pressureValue) {
            return receive( receiver.channelPressure(offset, port, channel, pressureValue));
        }
        public T channelPressure(double offset, MetroPort port, int channel, int pressureValue) {
            return receive( receiver.channelPressure(offset, port, channel, pressureValue));
        }
        public T pitchBend(double offset, MetroPort port, int channel, double pitchBendValue) {
            return receive( receiver.pitchBend(offset, port, channel, pitchBendValue));
        }
        public T pitchBend(double offset, MetroPort port, int channel, int pitchBendValue) {
            return receive( receiver.pitchBend(offset, port, channel, pitchBendValue));
        }
        public T cc_allSoundOff(double offset, MetroPort port, int channel) {
            return receive( receiver.cc_allSoundOff(offset, port, channel));
        }
        public T cc_resetAllControllers(double offset, MetroPort port, int channel) {
            return receive( receiver.cc_resetAllControllers(offset, port, channel));
        }
        public T cc_localControls(double offset, MetroPort port, int channel, boolean on) {
            return receive( receiver.cc_localControls(offset, port, channel, on));
        }
        public T cc_allNoteOff(double offset, MetroPort port, int channel) {
            return receive( receiver.cc_allNoteOff(offset, port, channel));
        }
        public T cc_omniModeOff(double offset, MetroPort port, int channel) {
            return receive( receiver.cc_omniModeOff(offset, port, channel));
        }
        public T cc_omniModeOn(double offset, MetroPort port, int channel) {
            return receive( receiver.cc_omniModeOn(offset, port, channel));
        }
        public T cc_monoModeOn(double offset, MetroPort port, int channel) {
            return receive( receiver.cc_monoModeOn(offset, port, channel));
        }
        public T cc_polyModeOn(double offset, MetroPort port, int channel) {
            return receive( receiver.cc_polyModeOn(offset, port, channel));
        }
        public T songPositionPointer(double offset, MetroPort port, int pos) {
            return receive( receiver.songPositionPointer(offset, port, pos));
        }
        public T songSelect(double offset, MetroPort port, int songNumber) {
            return receive( receiver.songSelect(offset, port, songNumber));
        }
        public T endOfExclusive(double offset, MetroPort port) {
            return receive( receiver.endOfExclusive(offset, port));
        }
        public T clock(double offset, MetroPort port) {
            return receive( receiver.clock(offset, port));
        }
        public T start(double offset, MetroPort port) {
            return receive( receiver.start(offset, port));
        }
        public T cont(double offset, MetroPort port) {
            return receive( receiver.cont(offset, port));
        }
        public T stop(double offset, MetroPort port) {
            return receive( receiver.stop(offset, port));
        }
        public T reset(double offset, MetroPort port) {
            return receive( receiver.reset(offset, port));
        }
        public T cc_bankSelect(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_bankSelect(offset, port, channel, value));
        }
        public T cc_modulation(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_modulation(offset, port, channel, value));
        }
        public T cc_breathController(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_breathController(offset, port, channel, value));
        }
        public T cc_footController(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_footController(offset, port, channel, value));
        }
        public T cc_portamentoTime(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_portamentoTime(offset, port, channel, value));
        }
        public T cc_dataEntryMsb(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_dataEntryMsb(offset, port, channel, value));
        }
        public T cc_volume(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_volume(offset, port, channel, value));
        }
        public T cc_balance(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_balance(offset, port, channel, value));
        }
        public T cc_pan(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_pan(offset, port, channel, value));
        }
        public T cc_expression(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_expression(offset, port, channel, value));
        }
        public T cc_effectController1(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_effectController1(offset, port, channel, value));
        }
        public T cc_effectController2(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_effectController2(offset, port, channel, value));
        }
        public T cc_sustainPedal(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_sustainPedal(offset, port, channel, value));
        }
        public T cc_portamentoSwitch(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_portamentoSwitch(offset, port, channel, value));
        }
        public T cc_sostenutoSwitch(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_sostenutoSwitch(offset, port, channel, value));
        }
        public T cc_pedalSwitch(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_pedalSwitch(offset, port, channel, value));
        }
        public T cc_legatoSwitch(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_legatoSwitch(offset, port, channel, value));
        }
        public T cc_hold2(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_hold2(offset, port, channel, value));
        }
        public T cc_soundController1(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController1(offset, port, channel, value));
        }
        public T cc_soundController2(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController2(offset, port, channel, value));
        }
        public T cc_soundController3(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController3(offset, port, channel, value));
        }
        public T cc_soundController4(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController4(offset, port, channel, value));
        }
        public T cc_soundController5(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController5(offset, port, channel, value));
        }
        public T cc_soundController6(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController6(offset, port, channel, value));
        }
        public T cc_soundController7(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController7(offset, port, channel, value));
        }
        public T cc_soundController8(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController8(offset, port, channel, value));
        }
        public T cc_soundController9(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController9(offset, port, channel, value));
        }
        public T cc_soundController10(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_soundController10(offset, port, channel, value));
        }
        public T cc_generalPurpose01(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_generalPurpose01(offset, port, channel, value));
        }
        public T cc_generalPurpose02(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_generalPurpose02(offset, port, channel, value));
        }
        public T cc_generalPurpose03(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_generalPurpose03(offset, port, channel, value));
        }
        public T cc_generalPurpose04(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_generalPurpose04(offset, port, channel, value));
        }
        public T cc_portamento(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_portamento(offset, port, channel, value));
        }
        public T cc_effect1(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_effect1(offset, port, channel, value));
        }
        public T cc_effect2(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_effect2(offset, port, channel, value));
        }
        public T cc_effect3(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_effect3(offset, port, channel, value));
        }
        public T cc_effect4(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_effect4(offset, port, channel, value));
        }
        public T cc_effect5(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_effect5(offset, port, channel, value));
        }
        public T cc_dataIncrement(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_dataIncrement(offset, port, channel, value));
        }
        public T cc_dataDecrement(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_dataDecrement(offset, port, channel, value));
        }
        public T cc_nrpnLsb(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_nrpnLsb(offset, port, channel, value));
        }
        public T cc_nrpnMsb(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_nrpnMsb(offset, port, channel, value));
        }
        public T cc_rpnLsb(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_rpnLsb(offset, port, channel, value));
        }
        public T cc_rpnMsb(double offset, MetroPort port, int channel, int value) {
            return receive( receiver.cc_rpnMsb(offset, port, channel, value));
        }
        public Default(MetroBufferedMidiReceiver<T> receiver) {
            super();
            this.receiver = receiver;
        }
        
        
    }
}

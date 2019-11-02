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
 * @see MetroMidiReceiver .
 * @author Ats Oka
 */
public interface MetroBufferedMidiReceiver<T> {
    public T error( double offset, MetroPort port, String message );

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
}

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
public interface MetroMidiBufferedReceiver {
    // basic 
    public void noteOn( double offset, MetroPort port, int channel, int note, double velocity );
    public void noteOn( double offset, MetroPort port, int channel, int note, int velocity );
    public void noteOff( double offset, MetroPort port, int channel, int note, double velocity);
    public void noteOff( double offset, MetroPort port, int channel, int note, int velocity );
    public void keyPressure( double offset, MetroPort port, int channel, int note, double pressure);
    public void keyPressure( double offset, MetroPort port, int channel, int note, int pressure);
    public void controlChange( double offset, MetroPort port, int channel, int controlNumber, int controlValue );
    public void programChange(  double offset, MetroPort port, int ch, int programNumber );
    public void channelPressure(  double offset, MetroPort port, int ch, double pressureValue );
    public void channelPressure(  double offset, MetroPort port, int ch, int pressureValue );
    public void pitchBend(  double offset, MetroPort port, int ch, double pitchBendValue );
    public void pitchBend(  double offset, MetroPort port, int ch, int pitchBendValue );

    public void cc_allSoundOff(  double offset, MetroPort port, int ch ); 
    public void cc_resetAllControllers(  double offset, MetroPort port, int ch ); 
    public void cc_localControls(  double offset, MetroPort port, int ch, boolean on ); 
    public void cc_allNoteOff(  double offset, MetroPort port, int ch ); 
    public void cc_omniModeOff(  double offset, MetroPort port, int ch ); 
    public void cc_omniModeOn(  double offset, MetroPort port, int ch ); 
    public void cc_monoModeOn(  double offset, MetroPort port, int ch ); 
    public void cc_polyModeOn(  double offset, MetroPort port, int ch ); 

    // system
    public void songPositionPointer( double offset, MetroPort port, int pos ); 
    public void songSelect( double offset, MetroPort port, int songNumber ); 
    public void endOfExclusive( double offset, MetroPort port ); 
    public void clock( double offset, MetroPort port ); 
    public void start( double offset, MetroPort port ); 
    public void cont( double offset, MetroPort port ); 
    public void stop( double offset, MetroPort port ); 
    public void reset( double offset, MetroPort port );

    // control change
    public void cc_bankSelect( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_modulation( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_breathController( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_footController( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_portamentoTime( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_dataEntryMsb( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_volume( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_balance( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_pan( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_expression( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_effectController1( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_effectController2( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_sustainPedal( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_portamentoSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_sostenutoSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_pedalSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_legatoSwitch( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_hold2( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController1( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController2( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController3( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController4( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController5( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController6( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController7( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController8( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController9( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_soundController10( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_generalPurpose01( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_generalPurpose02( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_generalPurpose03( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_generalPurpose04( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_portamento( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_effect1( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_effect2( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_effect3( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_effect4( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_effect5( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_dataIncrement( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_dataDecrement( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_nrpnLsb( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_nrpnMsb( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_rpnLsb( double offset, MetroPort port,  int channel, int value ) ;
    public void cc_rpnMsb( double offset, MetroPort port,  int channel, int value ) ;
}

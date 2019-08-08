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
public interface MetroBufferedMidiReceiver {
	// basic 
	public void noteOn( double offset, int port, int channel, int note, double velocity );
	public void noteOn( double offset, int port, int channel, int note, int velocity );
	public void noteOff( double offset, int port, int channel, int note, double velocity);
	public void noteOff( double offset, int port, int channel, int note, int velocity );
	public void keyPressure( double offset, int port, int channel, int note, double pressure);
	public void keyPressure( double offset, int port, int channel, int note, int pressure);
	public void controlChange( double offset, int port, int channel, int controlNumber, int controlValue );
	public void programChange(  double offset, int port, int ch, int programNumber );
	public void channelPressure(  double offset, int port, int ch, double pressureValue );
	public void channelPressure(  double offset, int port, int ch, int pressureValue );
	public void pitchBend(  double offset, int port, int ch, double pitchBendValue );
	public void pitchBend(  double offset, int port, int ch, int pitchBendValue );

	public void cc_allSoundOff(  double offset, int port, int ch ); 
	public void cc_resetAllControllers(  double offset, int port, int ch ); 
	public void cc_localControls(  double offset, int port, int ch, boolean on ); 
	public void cc_allNoteOff(  double offset, int port, int ch ); 
	public void cc_omniModeOff(  double offset, int port, int ch ); 
	public void cc_omniModeOn(  double offset, int port, int ch ); 
	public void cc_monoModeOn(  double offset, int port, int ch ); 
	public void cc_polyModeOn(  double offset, int port, int ch ); 

	// system
	public void songPositionPointer( double offset, int port, int pos ); 
	public void songSelect( double offset, int port, int songNumber ); 
	public void endOfExclusive( double offset, int port ); 
	public void clock( double offset, int port ); 
	public void start( double offset, int port ); 
	public void cont( double offset, int port ); 
	public void stop( double offset, int port ); 
	public void reset( double offset, int port );

	// control change
	public void cc_bankSelect( double offset, int port,  int channel, int value ) ;
	public void cc_modulation( double offset, int port,  int channel, int value ) ;
	public void cc_breathController( double offset, int port,  int channel, int value ) ;
	public void cc_footController( double offset, int port,  int channel, int value ) ;
	public void cc_portamentoTime( double offset, int port,  int channel, int value ) ;
	public void cc_dataEntryMsb( double offset, int port,  int channel, int value ) ;
	public void cc_volume( double offset, int port,  int channel, int value ) ;
	public void cc_balance( double offset, int port,  int channel, int value ) ;
	public void cc_pan( double offset, int port,  int channel, int value ) ;
	public void cc_expression( double offset, int port,  int channel, int value ) ;
	public void cc_effectController1( double offset, int port,  int channel, int value ) ;
	public void cc_effectController2( double offset, int port,  int channel, int value ) ;
	public void cc_sustainPedal( double offset, int port,  int channel, int value ) ;
	public void cc_portamentoSwitch( double offset, int port,  int channel, int value ) ;
	public void cc_sostenutoSwitch( double offset, int port,  int channel, int value ) ;
	public void cc_pedalSwitch( double offset, int port,  int channel, int value ) ;
	public void cc_legatoSwitch( double offset, int port,  int channel, int value ) ;
	public void cc_hold2( double offset, int port,  int channel, int value ) ;
	public void cc_soundController1( double offset, int port,  int channel, int value ) ;
	public void cc_soundController2( double offset, int port,  int channel, int value ) ;
	public void cc_soundController3( double offset, int port,  int channel, int value ) ;
	public void cc_soundController4( double offset, int port,  int channel, int value ) ;
	public void cc_soundController5( double offset, int port,  int channel, int value ) ;
	public void cc_soundController6( double offset, int port,  int channel, int value ) ;
	public void cc_soundController7( double offset, int port,  int channel, int value ) ;
	public void cc_soundController8( double offset, int port,  int channel, int value ) ;
	public void cc_soundController9( double offset, int port,  int channel, int value ) ;
	public void cc_soundController10( double offset, int port,  int channel, int value ) ;
	public void cc_generalPurpose01( double offset, int port,  int channel, int value ) ;
	public void cc_generalPurpose02( double offset, int port,  int channel, int value ) ;
	public void cc_generalPurpose03( double offset, int port,  int channel, int value ) ;
	public void cc_generalPurpose04( double offset, int port,  int channel, int value ) ;
	public void cc_portamento( double offset, int port,  int channel, int value ) ;
	public void cc_effect1( double offset, int port,  int channel, int value ) ;
	public void cc_effect2( double offset, int port,  int channel, int value ) ;
	public void cc_effect3( double offset, int port,  int channel, int value ) ;
	public void cc_effect4( double offset, int port,  int channel, int value ) ;
	public void cc_effect5( double offset, int port,  int channel, int value ) ;
	public void cc_dataIncrement( double offset, int port,  int channel, int value ) ;
	public void cc_dataDecrement( double offset, int port,  int channel, int value ) ;
	public void cc_nrpnLsb( double offset, int port,  int channel, int value ) ;
	public void cc_nrpnMsb( double offset, int port,  int channel, int value ) ;
	public void cc_rpnLsb( double offset, int port,  int channel, int value ) ;
	public void cc_rpnMsb( double offset, int port,  int channel, int value ) ;
}

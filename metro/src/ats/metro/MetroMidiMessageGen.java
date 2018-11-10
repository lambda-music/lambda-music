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

/**
 * https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message
 * @author ats
 */
final class MetroMidiMessageGen {
	//	NOTEOFF,
	//	NOTEON,
	//	KEYPRESSURE, // AKA AFTERTOUCH
	//	CONTROL,
	//	PROGRAM,
	//	CHANNELPRESSURE,
	//	PITCHBEND,

	public static byte[] noteOn( int ch, int note, int velo ) {
		return new byte[] {
				(byte) ( 0b10010000 | ( 0b00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( velo ), }; 
	}
	public static String toString( byte[] b ) {
		StringBuilder sb = new StringBuilder();
		for ( int i=0; i<b.length; i++ ) {
			sb.append("'" );
			sb.append( Integer.toString( Byte.toUnsignedInt( b[i] ), 16 ) );
			sb.append(" ");
			
		}
		return sb.toString();
	}
//	public static void main(String[] args) {
//		System.out.println( toString( noteOn( 5,64, 1 ) )  ) ;
//		System.out.println( 0b1111 & 4 );
//	}
	
	// A helper function
	public static byte[] noteOn( int ch, int note, double velo ) {
		return new byte[] {
				(byte) ( 0b10010000 | ( 0b00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( 127d * velo ), }; 
	}
	public static byte[] noteOff( int ch, int note, int velo ) {
		return new byte[] {
				(byte) ( 0b10000000 | ( 0b00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( velo ), }; 
	}
	// A helper function
	public static byte[] noteOff( int ch, int note, double velo ) {
		return new byte[] {
				(byte) ( 0b10000000 | ( 0b00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( 127d * velo ), }; 
	}
	
	public static byte[] keyPressure( int ch, int note, int pressure ) {
		return new byte[] {
				(byte) ( 0b10100000 | ( 0b00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( pressure ), }; 
	}
	public static byte[] keyPressure( int ch, int note, double pressure ) {
		return new byte[] {
				(byte) ( 0b10100000 | ( 0b00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( 255d * pressure ), }; 
	}
	
	public static byte[] controlChange( int ch, int controlNumber, int controlValue ) {
		return new byte[] {
				(byte) ( 0b10110000 | ( 0b00001111 & ch )), 
				(byte) ( controlNumber ), 
				(byte) ( controlValue ), }; 
	}
	public static byte[] programChange( int ch, int programNumber ) {
		return new byte[] {
				(byte) ( 0b11000000 | ( 0b00001111 & ch )), 
				(byte) ( programNumber ), 
				};
	}
	public static byte[] channelPressure( int ch, int pressureValue ) {
		return new byte[] {
				(byte) ( 0b11010000 | ( 0b00001111 & ch )), 
				(byte) ( pressureValue ), 
				};
	}
	public static byte[] channelPressure( int ch, double pressureValue ) {
		return new byte[] {
				(byte) ( 0b11010000 | ( 0b00001111 & ch )), 
				(byte) ( 255d * pressureValue ), 
				};
	}
	public static byte[] pitchBend( int ch, int pitchBendValue ) {
//		System.out.println( "pitchBendValue:" + pitchBendValue );
//		pitchBendValue += 0x2000;
		return new byte[] {
				(byte) ( 0b11100000 | ( 0b00001111 & ch )), 
				(byte) ( 0b01111111 & pitchBendValue ), 
				(byte) ( 0b01111111 & ( pitchBendValue >>> 7 ) ), 
				}; 
	}
	
	/**
	 * Specifying pitch bend value by a double-float numeric value. The resolution
	 * relies on only fourteen bits; thus any value change less than 0.0001 
	 * ( 1.0d/8192 ≒ 0.0001 ) will not have any effect on output values.
	 */
	public static byte[] pitchBend( int ch, double pitchBendValue ) {
		return pitchBend( ch, 0x2000  + (int)((double)0x2000 * pitchBendValue) ); 
	}
	
	public static byte[] cc_allSoundOff( int ch ) {
		return controlChange(ch, 120, 0 ); 
	}
	public static byte[] cc_resetAllControllers( int ch ) {
		return controlChange(ch, 121, 0 ); 
	}
	public static byte[] cc_localControls( int ch, boolean on ) {
		return controlChange(ch, 122, on ? 127 : 0 ); 
	}
	public static byte[] cc_allNoteOff( int ch ) {
		return controlChange(ch, 123, 0 ); 
	}
	public static byte[] cc_omniModeOff( int ch ) {
		return controlChange(ch, 124, 0 ); 
	}
	public static byte[] cc_omniModeOn( int ch ) {
		return controlChange(ch, 125, 0 ); 
	}
	public static byte[] cc_monoModeOn( int ch ) {
		return controlChange(ch, 126, 0 ); 
	}
	public static byte[] cc_polyModeOn( int ch ) {
		return controlChange(ch, 127, 0 ); 
	}
	
	//// SYSTEM

	public static byte[] songPositionPointer( int pos ) {
		return new byte[] {
				(byte) ( 0b11110010 ), 
				(byte) ( 0b01111111 & ( pos       ) ), 
				(byte) ( 0b01111111 & ( pos >>> 7 ) ), 
				}; 
	}
	public static byte[] songSelect( int songNumber ) {
		return new byte[] {
				(byte) ( 0b11110011 ), 
				(byte) ( 0b01111111 & ( songNumber       ) ) 
				}; 
	}
	
	public static byte[] endOfExclusive() {
		return new byte[] {
				(byte) ( 0b11110111 ), 
		}; 
	}
	public static byte[] clock() {
		return new byte[] {
				(byte) ( 0b11111000 ), 
		}; 
	}
	public static byte[] start() {
		return new byte[] {
				(byte) ( 0b11111010 ), 
		}; 
	}
	public static byte[] cont() {
		return new byte[] {
				(byte) ( 0b11111011 ), 
		}; 
	}
	public static byte[] stop() {
		return new byte[] {
				(byte) ( 0b11111100 ), 
		}; 
	}
	public static byte[] reset() {
		return new byte[] {
				(byte) ( 0b11111111 ), 
		}; 
	}
	
	public static <T> T receive( MetroMidiReceiver<T> receiver, byte[] message ) {
		if ( message == null || message.length == 0 ) {
			return receiver.error( "no data" );
		}
		
		int command  = ( 0b11110000 & message[0] );
		int channel  = ( 0b11110000 & message[0] );
		switch ( command ) {
			case 0b10010000 : 
				return receiver.noteOn(  channel, message[1], message[2] );
			
			case 0b10000000 : 
				return receiver.noteOff( channel, message[1], message[2] );
			
			case 0b10100000 : 
				return receiver.keyPressure( channel, message[1], message[2] );

			case 0b11000000 : 
				return receiver.programChange( channel, message[1] );
			case 0b11010000 : 
				return receiver.channelPressure( channel, message[1] );
			case 0b11100000 : 
				return receiver.pitchBend( channel,
						( 0b01111111 & message[1] ) | 
						( (0b01111111 & message[2] ) << 7 ) );

			case 0b11110000 : 
				/* 
				 * The followings are special channel mode messages which
				 * supposed to be processed by all channels.  the channel value
				 * is used as command identifier.
				 */
				switch ( channel ) {
					case 0b00000010 :
						return receiver.songPositionPointer(
								( 0b01111111 & message[1] ) | 
								( (0b01111111 & message[2] ) << 7 ) );
					case 0b00000011 :
						return receiver.songSelect( 0b01111111 & message[1] );
					case 0b00000111 :
						return receiver.endOfExclusive() ;
					case 0b00001000 :
						return receiver.clock() ;
					case 0b00001010 :
						return receiver.start() ;
					case 0b00001011 :
						return receiver.cont() ;
					case 0b00001100 :
						return receiver.stop() ;
					case 0b00001111 :
						return receiver.reset() ;
					default :
						return receiver.error( "unknown control change for all channels" );
				}

			case 0b10110000 : {
				T value = receiver.controlChange( channel, message[1], message[2] ) ;
				if ( value != null )
					return value;
			
				switch ( message[1] ) {
					/*
					 * Channel Mode Messages
					 */
					case MetroMidi.CC_ALL_SOUND_OFF : // case 120 :
						return receiver.cc_allSoundOff( channel ); 
					case MetroMidi.CC_RESET_ALL_CONTROLLERS : // case 121
						return receiver.cc_resetAllControllers( channel ); 
					case MetroMidi.CC_LOCAL_CONTROLS : // case 122 :
						return receiver.cc_localControls( channel, message[2] != 0 ); 
					case MetroMidi.CC_ALL_NOTE_OFF : // case 123 :
						return receiver.cc_allNoteOff( channel ); 
					case MetroMidi.CC_OMNI_MODE_OFF : // case 124 :
						return receiver.cc_omniModeOff( channel ); 
					case MetroMidi.CC_OMNI_MODE_ON : // case 125 :
						return receiver.cc_omniModeOn( channel ); 
					case MetroMidi.CC_MONO_MODE_ON : // case 126 :
						return receiver.cc_monoModeOn( channel ); 
					case MetroMidi.CC_POLY_MODE_ON : // case 127 :
						return receiver.cc_polyModeOn( channel );
						
					/*
					 * Normal Messages
					 */
					case MetroMidi.CC_BANK_SELECT :
						return receiver.cc_bankSelect( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_MODULATION :
						return receiver.cc_modulation( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_BREATH_CTRL :
						return receiver.cc_breathController( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_FOOT_CTRL :
						return receiver.cc_footController( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_PORTAMENTO_TIME :
						return receiver.cc_portamentoTime( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_DATA_ENTRY_MSB :
						return receiver.cc_dataEntryMsb( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_VOLUME :
						return receiver.cc_volume( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_BALANCE :
						return receiver.cc_balance( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_PAN :
						return receiver.cc_pan( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EXPRESSION :
						return receiver.cc_expression( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EFFECT_CTRL_1 :
						return receiver.cc_effectController1( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EFFECT_CTRL_2 :
						return receiver.cc_effectController2( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SUSTAIN_PEDAL :
						return receiver.cc_sustainPedal( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_PORTAMENTO_SWITCH :
						return receiver.cc_portamentoSwitch( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOSTENUTO_SWITCH :
						return receiver.cc_sostenutoSwitch( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOFT_PEDAL_SWITCH :
						return receiver.cc_pedalSwitch( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_LEGATO_FOOTSWITCH :
						return receiver.cc_legatoSwitch( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_HOLD_2 :
						return receiver.cc_hold2( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_01 :
						return receiver.cc_soundController1( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_02 :
						return receiver.cc_soundController2( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_03 :
						return receiver.cc_soundController3( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_04 :
						return receiver.cc_soundController4( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_05 :
						return receiver.cc_soundController5( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_06 :
						return receiver.cc_soundController6( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_07 :
						return receiver.cc_soundController7( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_08 :
						return receiver.cc_soundController8( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_09 :
						return receiver.cc_soundController9( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_SOUND_CTRL_10 :
						return receiver.cc_soundController10( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_GENERAL_PURPOSE_01 :
						return receiver.cc_generalPurpose01( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_GENERAL_PURPOSE_02 :
						return receiver.cc_generalPurpose02( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_GENERAL_PURPOSE_03 :
						return receiver.cc_generalPurpose03( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_GENERAL_PURPOSE_04 :
						return receiver.cc_generalPurpose04( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_PORTAMENTO_CC_CTRL :
						return receiver.cc_portamento( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EFFECT_1_DEPTH :
						return receiver.cc_effect1( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EFFECT_2_DEPTH :
						return receiver.cc_effect2( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EFFECT_3_DEPTH :
						return receiver.cc_effect3( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EFFECT_4_DEPTH :
						return receiver.cc_effect4( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_EFFECT_5_DEPTH :
						return receiver.cc_effect5( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_DATA_INCREMENT :
						return receiver.cc_dataIncrement( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_DATA_DECREMENT :
						return receiver.cc_dataDecrement( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_NRPN_LSB :
						return receiver.cc_nrpnLsb( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_NRPN_MSB :
						return receiver.cc_nrpnMsb( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_RPN_LSB :
						return receiver.cc_rpnLsb( channel, 0xff & message[2] ) ;

					case MetroMidi.CC_RPN_MSB :
						return receiver.cc_rpnMsb( channel, 0xff & message[2] ) ;

					default :
						return receiver.error( "unknown control change" );
				}
			}
		}
		
		return receiver.error( "Unknown Type" );
	}

	public static String toStri(byte[] bytes) {
	    StringBuilder sb = new StringBuilder();
	    sb.append("[ ");
	    for (byte b : bytes) {
	        sb.append(String.format("0x%02X ", b));
	    }
	    sb.append("]");
	    return sb.toString();
	}
	
	public static void main(String[] args) {
//		byte b = (byte) 0b11111100;
//		int i = 0b11110000 & b;
//		System.out.println(    i   );
//		System.out.println(    0b11110000  );
		receive( 
				new MetroMidiReceiver.Default<Integer>() {
					@Override
					protected Integer defaultValue() {
						return null;
					}
					@Override
					public Integer pitchBend(int ch, int pitchBendValue) {
						System.out.println( Integer.toString( pitchBendValue, 16 ) );
						return  super.pitchBend(ch, pitchBendValue);
					}
				}, pitchBend(1, 0x1ff0 ) );
		
		System.out.println(  toStri( pitchBend(1, 0x1ff0 ) ) );
	}
	

	static class Test {
		public static void main(String[] args) {
			System.out.println( String.format( "%4.4f", 1d/8192  ));
//		test(  pitchBend( 1, 0x2000 ) );
//		test(  pitchBend( 1, 0x2001 ) );
//		test(  pitchBend( 1, 0x2003 ) );
//		test(  pitchBend( 1, 0x1fff ) );
//		test(  pitchBend( 1, 0x1ffe ) );
			test(  pitchBend( 1,    0d ) );
			test(  pitchBend( 1,  1d/8192 ) );
			test(  pitchBend( 1, -1d/8192   ) );
			
		}
		
		private static void test(byte[] pitchBend) {
			System.out.println("");
			for ( int i=0; i<pitchBend.length; i++ ) {
				System.out.println( String.format( "%8.8s", Integer.toBinaryString( pitchBend[i] & 0xff ) ));
			}
		}
	}
}

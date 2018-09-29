package ats.metro;

/**
 * https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message
 * @author ats
 */
public final class MetroMidi {
	//	NOTEOFF,
	//	NOTEON,
	//	KEYPRESSURE, // AKA AFTERTOUCH
	//	CONTROL,
	//	PROGRAM,
	//	CHANNELPRESSURE,
	//	PITCHBEND,

	public static byte[] noteOn( int ch, int note, int velo ) {
		return new byte[] {
				(byte) ( 0b10010000 | ( 0x00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( velo ), }; 
	}
	// A helper function
	public static byte[] noteOn( int ch, int note, double velo ) {
		return new byte[] {
				(byte) ( 0b10010000 | ( 0x00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( 127d * velo ), }; 
	}
	public static byte[] noteOff( int ch, int note, int velo ) {
		return new byte[] {
				(byte) ( 0b10000000 | ( 0x00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( velo ), }; 
	}
	// A helper function
	public static byte[] noteOff( int ch, int note, double velo ) {
		return new byte[] {
				(byte) ( 0b10000000 | ( 0x00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( 127d * velo ), }; 
	}
	
	public static byte[] keyPressure( int ch, int note, int pressure ) {
		return new byte[] {
				(byte) ( 0b10100000 | ( 0x00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( pressure ), }; 
	}
	public static byte[] keyPressure( int ch, int note, double pressure ) {
		return new byte[] {
				(byte) ( 0b10100000 | ( 0x00001111 & ch )), 
				(byte) ( note ), 
				(byte) ( 255d * pressure ), }; 
	}
	
	public static byte[] control( int ch, int controlNumber, int controlValue ) {
		return new byte[] {
				(byte) ( 0b10110000 | ( 0x00001111 & ch )), 
				(byte) ( controlNumber ), 
				(byte) ( controlValue ), }; 
	}
	public static byte[] program( int ch, int programNumber ) {
		return new byte[] {
				(byte) ( 0b11000000 | ( 0x00001111 & ch )), 
				(byte) ( programNumber ), 
				};
	}
	public static byte[] channelPressure( int ch, int pressureValue ) {
		return new byte[] {
				(byte) ( 0b11010000 | ( 0x00001111 & ch )), 
				(byte) ( pressureValue ), 
				};
	}
	public static byte[] channelPressure( int ch, double pressureValue ) {
		return new byte[] {
				(byte) ( 0b11010000 | ( 0x00001111 & ch )), 
				(byte) ( 255d * pressureValue ), 
				};
	}
	public static byte[] pitchBend( int ch, int pitchBendValue ) {
//		System.out.println( "pitchBendValue:" + pitchBendValue );
//		pitchBendValue += 0x2000;
		return new byte[] {
				(byte) ( 0b11100000 | ( 0x00001111 & ch )), 
				(byte) ( 0b01111111 & pitchBendValue ), 
				(byte) ( 0b01111111 & ( pitchBendValue >>> 7 ) ), 
				}; 
	}
	
	/**
	 * Specifying pitch bend value by a double-float numeric value. The resolution
	 * relies on only fourteen bits; thus any value change less than 0.0001 
	 * ( 1.0d/8192 = 0.0001 ) will not have any effect on output values.
	 */
	public static byte[] pitchBend( int ch, double pitchBendValue ) {
		return pitchBend( ch, 0x2000  + (int)((double)0x2000 * pitchBendValue) ); 
	}
	
	public static byte[] cc_allSoundOff( int ch ) {
		return control(ch, 120, 0 ); 
	}
	public static byte[] cc_resetAllControllers( int ch ) {
		return control(ch, 121, 0 ); 
	}
	public static byte[] cc_localControls( int ch, boolean on ) {
		return control(ch, 122, on ? 127 : 0 ); 
	}
	public static byte[] cc_allNoteOff( int ch ) {
		return control(ch, 123, 0 ); 
	}
	public static byte[] cc_omniModeOff( int ch ) {
		return control(ch, 124, 0 ); 
	}
	public static byte[] cc_omniModeOn( int ch ) {
		return control(ch, 125, 0 ); 
	}
	public static byte[] cc_monoModeOff( int ch ) {
		return control(ch, 126, 0 ); 
	}
	public static byte[] cc_polyModeOn( int ch ) {
		return control(ch, 127, 0 ); 
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

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
 * See https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message
 * 
 * The method {@link MetroMidiMessages#receive(MetroMidiReceiver, byte[])} plays the important role
 * for Various Midi receivers such as {@link MetroMidiReceiver} and {@link MetroBufferedMidiReceiver}.
 * See {@link MetroMidiReceiver} and {@link MetroBufferedMidiReceiver}.
 *  
 * @author ats
 */
public final class MetroMidiMessages {
    //  NOTEOFF,
    //  NOTEON,
    //  KEYPRESSURE, // AKA AFTERTOUCH
    //  CONTROL,
    //  PROGRAM,
    //  CHANNELPRESSURE,
    //  PITCHBEND,

    public static byte[] noteOn( int ch, int note, int velo ) {
        return new byte[] {
                (byte) ( 0b10010000 | ( 0b00001111 & ch )), 
                (byte) ( 0b01111111 & note ), 
                (byte) ( 0b01111111 & velo ), }; 
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
//  public static void main(String[] args) {
//      System.out.println( toString( noteOn( 5,64, 1 ) )  ) ;
//      System.out.println( 0b1111 & 4 );
//  }

    // (Sat, 02 Nov 2019 05:09:25 +0900)
    // hopefully following four methods are inlined at compile time. 
    private static final int d2i_7bit(double velo) {
        return 0b01111111 & (int)(127d * velo);
    }
    private static double i2d_7bit(int velo) {
        return (((double)velo) / 127d);
    }
    private static final int d2i_14bit(double pitchBendValue) {
        return 0x2000  + (int)(((double)0x2000) * pitchBendValue);
    }
    // not tested yet (Sat, 02 Nov 2019 05:21:53 +0900)
    private static final double i2d_14bit(int pitchBendValue) {
        return  (((double)pitchBendValue) / ((double)0x2000) ) - 1.0d;
    }

    public static final int d2iVelocity( double velo ) {
        return d2i_7bit( velo ); 
    }
    public static final double i2dVelocity( int velo ) {
        return i2d_7bit( velo ); 
    }
    // A helper function
    public static byte[] noteOn( int ch, int note, double velo ) {
//        System.err.println( "noteOn:" + velo );
        return new byte[] {
                (byte) ( 0b10010000 | ( 0b00001111 & ch )), 
                (byte) ( note ), 
                (byte) d2i_7bit( velo ), }; 
    }
    public static byte[] noteOff( int ch, int note, int velo ) {
        return new byte[] {
                (byte) ( 0b10000000 | ( 0b00001111 & ch )), 
                (byte) ( note ), 
                (byte) ( 0b01111111 & velo ), }; 
    }
    // A helper function
    public static byte[] noteOff( int ch, int note, double velo ) {
        return new byte[] {
                (byte) ( 0b10000000 | ( 0b00001111 & ch )), 
                (byte) ( note ), 
                (byte) d2i_7bit( velo ), }; 
    }

    public static final int d2iPressure( double velo ) {
        return d2i_7bit( velo ); 
    }
    public static final double i2dPressure( int velo ) {
        return i2d_7bit( velo ); 
    }

    public static byte[] keyPressure( int ch, int note, int pressure ) {
        return new byte[] {
                (byte) ( 0b10100000 | ( 0b00001111 & ch )), 
                (byte) ( note ), 
                (byte) ( 0b01111111 & pressure ), }; 
    }
    public static byte[] keyPressure( int ch, int note, double pressure ) {
        return new byte[] {
                (byte) ( 0b10100000 | ( 0b00001111 & ch )), 
                (byte) ( note ), 
                (byte) d2i_7bit( pressure ), }; 
    }
    
    public static byte[] controlChange( int ch, int controlNumber, int controlValue ) {
        return new byte[] {
                (byte) ( 0b10110000 | ( 0b00001111 & ch )), 
                (byte) ( 0b01111111 & controlNumber ), 
                (byte) ( 0b01111111 & controlValue  ), }; 
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
                (byte) ( 0b01111111 & pressureValue ), 
                };
    }
    public static byte[] channelPressure( int ch, double pressureValue ) {
        return new byte[] {
                (byte) ( 0b11010000 | ( 0b00001111 & ch )), 
                (byte) d2i_7bit( pressureValue ), 
                };
    }
    public static byte[] pitchBend( int ch, int pitchBendValue ) {
//      System.out.println( "pitchBendValue:" + pitchBendValue );
//      pitchBendValue += 0x2000;
        return new byte[] {
                (byte) ( 0b11100000 | ( 0b00001111 & ch )), 
                (byte) ( 0b01111111 & pitchBendValue ), 
                (byte) ( 0b01111111 & ( pitchBendValue >>> 7 ) ), 
                }; 
    }

    public static final int d2iPitchBend(double pitchBendValue) {
        return d2i_14bit( pitchBendValue );
    }
    public static final double i2dPitchBend(int pitchBendValue) {
        return i2d_14bit( pitchBendValue );
    }

    /**
     * Specifying pitch bend value by a double-float numeric value. 
     * Range is ( -1 <= pitchBendValue <= 1 ) 
     * The resolution relies on only fourteen bits; thus any value change 
     * less than 0.0001 ( 1.0d/8192 :=: 0.0001 ) will not have any effect 
     * on the output value.
     */
    public static byte[] pitchBend( int ch, double pitchBendValue ) {
        return pitchBend( ch, d2i_14bit( pitchBendValue ) ); 
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
    
    /**
     * See the documentation of the class {@link MetroMidiMessages};
     * Note that receivers' methods could return null.
     * 
     * @param receiver
     * @param message
     * @return
     */
    public static <T> void receive( MetroCollector<T> result, MetroMidiReceiver<T> receiver, byte[] message) {
        if ( message == null || message.length == 0 ) {
            receiver.error( result, "no data" );
        }
        
        int command  = ( 0b11110000 & message[0] );
        int channel  = ( 0b00001111 & message[0] );
        switch ( command ) {
            case 0b10010000 : 
                receiver.noteOn( result, channel, message[1], message[2] );
                return;
            
            case 0b10000000 : 
                receiver.noteOff( result, channel, message[1], message[2] );
                return;
            
            case 0b10100000 : 
                receiver.keyPressure( result, channel, message[1], message[2] );
                return;

            case 0b11000000 : 
                receiver.programChange( result, channel, message[1] );
                return;
            case 0b11010000 : 
                receiver.channelPressure( result, channel, message[1] );
                return;
            case 0b11100000 : 
                receiver.pitchBend( result, channel,
                        ( 0b01111111 & message[1] ) | 
                        ( (0b01111111 & message[2] ) << 7 ) );
                return;
            case 0b11110000 : 
                /* 
                 * The followings are special channel mode messages which
                 * supposed to be processed by all channels.  the channel value
                 * is used as command identifier.
                 */
                switch ( channel ) {
                    case 0b00000010 :
                        receiver.songPositionPointer(result,
                                ( 0b01111111 & message[1] ) | 
                                ( (0b01111111 & message[2] ) << 7 ) );
                        return;
                    case 0b00000011 :
                        receiver.songSelect( result, 0b01111111 & message[1] );
                        return;
                    case 0b00000111 :
                        receiver.endOfExclusive(result) ;
                        return;
                    case 0b00001000 :
                        receiver.clock(result);
                        return;
                    case 0b00001010 :
                        receiver.start(result);
                        return;
                    case 0b00001011 :
                        receiver.cont(result);
                        return;
                    case 0b00001100 :
                        receiver.stop(result);
                        return;
                    case 0b00001111 :
                        receiver.reset(result);
                        return;
                    default :
                        receiver.error(result, "unknown control change for all channels" );
                        return;
                }

            case 0b10110000 : {
                // TODO !!
                receiver.controlChange(result, channel, message[1], message[2] ) ;
            
                switch ( message[1] ) {
                    /*
                     * Channel Mode Messages
                     */
                    case MetroMidi.CC_ALL_SOUND_OFF : // case 120 :
                        receiver.cc_allSoundOff( result, channel ); 
                        return;
                    case MetroMidi.CC_RESET_ALL_CONTROLLERS : // case 121
                        receiver.cc_resetAllControllers( result, channel ); 
                        return;
                    case MetroMidi.CC_LOCAL_CONTROLS : // case 122 :
                        receiver.cc_localControls( result, channel, message[2] != 0 ); 
                        return;
                    case MetroMidi.CC_ALL_NOTE_OFF : // case 123 :
                        receiver.cc_allNoteOff( result, channel ); 
                        return;
                    case MetroMidi.CC_OMNI_MODE_OFF : // case 124 :
                        receiver.cc_omniModeOff( result, channel ); 
                        return;
                    case MetroMidi.CC_OMNI_MODE_ON : // case 125 :
                        receiver.cc_omniModeOn( result, channel ); 
                        return;
                    case MetroMidi.CC_MONO_MODE_ON : // case 126 :
                        receiver.cc_monoModeOn( result, channel ); 
                        return;
                    case MetroMidi.CC_POLY_MODE_ON : // case 127 :
                        receiver.cc_polyModeOn( result, channel );
                        return;
                        
                    /*
                     * Normal Messages
                     */
                    case MetroMidi.CC_BANK_SELECT :
                        receiver.cc_bankSelect( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_MODULATION :
                        receiver.cc_modulation( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_BREATH_CTRL :
                        receiver.cc_breathController( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_FOOT_CTRL :
                        receiver.cc_footController( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_PORTAMENTO_TIME :
                        receiver.cc_portamentoTime( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_DATA_ENTRY_MSB :
                        receiver.cc_dataEntryMsb( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_VOLUME :
                        receiver.cc_volume( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_BALANCE :
                        receiver.cc_balance( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_PAN :
                        receiver.cc_pan( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EXPRESSION :
                        receiver.cc_expression( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EFFECT_CTRL_1 :
                        receiver.cc_effectController1( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EFFECT_CTRL_2 :
                        receiver.cc_effectController2( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SUSTAIN_PEDAL :
                        receiver.cc_sustainPedal( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_PORTAMENTO_SWITCH :
                        receiver.cc_portamentoSwitch( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOSTENUTO_SWITCH :
                        receiver.cc_sostenutoSwitch( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOFT_PEDAL_SWITCH :
                        receiver.cc_pedalSwitch( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_LEGATO_FOOTSWITCH :
                        receiver.cc_legatoSwitch( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_HOLD_2 :
                        receiver.cc_hold2( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_01 :
                        receiver.cc_soundController1( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_02 :
                        receiver.cc_soundController2( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_03 :
                        receiver.cc_soundController3( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_04 :
                        receiver.cc_soundController4( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_05 :
                        receiver.cc_soundController5( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_06 :
                        receiver.cc_soundController6( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_07 :
                        receiver.cc_soundController7( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_08 :
                        receiver.cc_soundController8( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_09 :
                        receiver.cc_soundController9( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_SOUND_CTRL_10 :
                        receiver.cc_soundController10( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_GENERAL_PURPOSE_01 :
                        receiver.cc_generalPurpose01( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_GENERAL_PURPOSE_02 :
                        receiver.cc_generalPurpose02( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_GENERAL_PURPOSE_03 :
                        receiver.cc_generalPurpose03( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_GENERAL_PURPOSE_04 :
                        receiver.cc_generalPurpose04( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_PORTAMENTO_CC_CTRL :
                        receiver.cc_portamento( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EFFECT_1_DEPTH :
                        receiver.cc_effect1( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EFFECT_2_DEPTH :
                        receiver.cc_effect2( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EFFECT_3_DEPTH :
                        receiver.cc_effect3( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EFFECT_4_DEPTH :
                        receiver.cc_effect4( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_EFFECT_5_DEPTH :
                        receiver.cc_effect5( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_DATA_INCREMENT :
                        receiver.cc_dataIncrement( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_DATA_DECREMENT :
                        receiver.cc_dataDecrement( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_NRPN_LSB :
                        receiver.cc_nrpnLsb( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_NRPN_MSB :
                        receiver.cc_nrpnMsb( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_RPN_LSB :
                        receiver.cc_rpnLsb( result, channel, 0xff & message[2] ) ;
                        return;

                    case MetroMidi.CC_RPN_MSB :
                        receiver.cc_rpnMsb( result, channel, 0xff & message[2] ) ;
                        return;

                    default :
                        receiver.error( result, "unknown control change" );
                        return;
                }
            }
        }
        
        receiver.error( result, "Unknown Type" );
        return;
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
        receive(
            (v)->System.out.println(v), 
            new MetroMidiReceiver.Default<String>() {
                @Override
                protected String defaultValue() {
                    return "";
                }
                @Override
                public void pitchBend(MetroCollector<String> result, int ch, int pitchBendValue) {
                    result.collect( String.format( "Channel : %x , Value : %x", ch ,  pitchBendValue ));
                }
            }, 
            pitchBend(1, 0x1ff0 ));
        
        System.out.println( toStri( pitchBend(1, 0x1ff0 ) ) );
    }
    

    static class Test {
        public static void main(String[] args) {
            System.out.println( String.format( "%4.4f", 1d/8192  ));
//      test(  pitchBend( 1, 0x2000 ) );
//      test(  pitchBend( 1, 0x2001 ) );
//      test(  pitchBend( 1, 0x2003 ) );
//      test(  pitchBend( 1, 0x1fff ) );
//      test(  pitchBend( 1, 0x1ffe ) );
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

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

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;
import java.util.logging.Logger;

/** @formatter:off */
/**
 * This class defines constant objects to generate a byte array which could
 * represent various MIDI messages.
 * <p>
 * Each of the constant objects has a method which name is "notifyMidiEvent".
 * This method generates a byte array which contains MIDI message.
 * <p>
 * Since the parameters of a notifyMidiEvent method differ by the target MIDI
 * messages of the method, we cannot define an abstract common method for the
 * notifyMidiEvent method. Therefore, we merely defines a convention  that the
 * constant objects defined here must contain a method which name is
 * "notifyMidiEvent".
 * <p>
 * 
 * See <a href=
 * "http://nickfever.com/music/midi-cc-list">http://nickfever.com/music/midi-cc-list</a>
 * for further information about MIDI messages.
 * <p>
 * When I wrote this class, I was looking for the official specification of MIDI
 * control change messages. But I could not find it. It seems that MIDI control
 * change messages are implicitly defined between those commercial products and
 * there is no officially declared specification for it.
 * <p>
 * Therefore, I referred <a href=
 * "http://nickfever.com/music/midi-cc-list">http://nickfever.com/music/midi-cc-list</a>.
 * <p>
 * This is a very good explanation about MIDI control change messages. But as
 * this article is not official, this might contain mistakes.
 * <p>
 * The following table is a quotation from the site.
 * <p>
 *
 * 
 * <table border="1" cellpadding="2">
 *  <tr><td>0</td>                                              <td>"Bank Select"</td>                                      <td>"Allows user to switch bank for patch selection. Program change used with Bank Select. MIDI can access 16,384 patches per MIDI channel."</td></tr>
 *  <tr><td>1</td>                                              <td>"Modulation"</td>                                       <td>"Generally this CC controls a vibrato effect (pitch, loudness, brightness ). What is modulated is based on the patch."</td></tr>
 *  <tr><td>2</td>                                              <td>"Breath Controller"</td>                                <td>"Often times associated with aftertouch messages. It was originally intended for use with a breath MIDI controller in which blowing harder produced higher MIDI control values. It can be used for modulation as well."</td></tr>
 *  <tr><td>4</td>                                              <td>"Foot Controller"</td>                                  <td>"Often used with aftertouch messages. It can send a continuous stream of values based on how the pedal is used."</td></tr>
 *  <tr><td>5</td>                                              <td>"PortamentoTime"</td>                                   <td>"Controls portamento rate to slide between 2 notes played subsequently."</td></tr>
 *  <tr><td>6</td>                                              <td>"Data InitializerEntry Most Significant Bit(MSB)"</td>             <td>"Controls Value for NRPN or RPN parameters."</td></tr>
 *  <tr><td>7</td>                                              <td>"Volume"</td>                                           <td>"Control the volume of the channel"</td></tr>
 *  <tr><td>8</td>                                              <td>"Balance"</td>                                          <td>"Controls the left and right balance, generally for stereo patches.0 = hard left, 64 = center, 127 = hard right"</td></tr>
 *  <tr><td>10</td>                                             <td>"Pan"</td>                                              <td>"Controls the left and right balance, generally for mono patches.0 = hard left, 64 = center, 127 = hard right"</td></tr>
 *  <tr><td>11</td>                                             <td>"Expression"</td>                                       <td>"Expression is a percentage of volume (CC7)."</td></tr>
 *  <tr><td>12</td>                                             <td>"Effect Controller 1"</td>                              <td>"Usually used to control a parameter of an effect within the synth/workstation."</td></tr>
 *  <tr><td>13</td>                                             <td>"Effect Controller 2"</td>                              <td>"Usually used to control a parameter of an effect within the synth/workstation."</td></tr>
 *  <tr><td>64</td>                                             <td>"Damper Pedal /Sustain Pedal"</td>                      <td>"On/Off switch that controls sustain. (See also Sostenuto CC 66)0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>65</td>                                             <td>"Portamento On/Off Switch"</td>                         <td>"On/Off switch0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>66</td>                                             <td>"Sostenuto On/Off Switch"</td>                          <td>"On/Off switch – Like the Sustain controller (CC 64), However it only holds notes that were “On” when the pedal was pressed. People use it to “hold” chords” and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>67</td>                                             <td>"Soft Pedal On/Off Switch"</td>                         <td>"On/Off switch- Lowers the volume of notes played.0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>68</td>                                             <td>"Legato FootSwitch"</td>                                <td>"On/Off switch- Turns Legato effect between 2 subsequent notes On or Off.0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>69</td>                                             <td>"Hold 2"</td>                                           <td>"Another way to “hold notes” (see MIDI CC 64 and MIDI CC 66). However notes fade out according to their release parameter rather than when the pedal is released."</td></tr>
 *  <tr><td>70</td>                                             <td>"Sound Controller 1"</td>                               <td>"Usually controls the way a sound is produced. Default = Sound Variation."</td></tr>
 *  <tr><td>71</td>                                             <td>"Sound Controller 2"</td>                               <td>"Allows shaping the Voltage Controlled Filter (VCF). Default = Resonance -also(Timbre or Harmonics)"</td></tr>
 *  <tr><td>72</td>                                             <td>"Sound Controller 3"</td>                               <td>"Controls release time of the Voltage controlled Amplifier (VCA). Default = Release Time."</td></tr>
 *  <tr><td>73</td>                                             <td>"Sound Controller 4"</td>                               <td>"Controls the “Attack’ of a sound. The attack is the amount of time it takes forthe sound to reach maximum amplitude."</td></tr>
 *  <tr><td>74</td>                                             <td>"Sound Controller 5"</td>                               <td>"Controls VCFs cutoff frequency of the filter."</td></tr>
 *  <tr><td>75</td>                                             <td>"Sound Controller 6"</td>                               <td>"Generic – Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>76</td>                                             <td>"Sound Controller 7"</td>                               <td>"Generic – Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>77</td>                                             <td>"Sound Controller 8"</td>                               <td>"Generic – Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>78</td>                                             <td>"Sound Controller 9"</td>                               <td>"Generic – Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>79</td>                                             <td>"Sound Controller 10"</td>                              <td>"Generic – Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>80</td>                                             <td>"General PurposeMIDI CC Controller"</td>                <td>"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>81</td>                                             <td>"General Purpose MIDI CC Controller"</td>               <td>"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>82</td>                                             <td>"General PurposeMIDI CC Controller"</td>                <td>"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>83</td>                                             <td>"General Purpose MIDI CC Controller"</td>               <td>"GenericOn/Off switch0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>84</td>                                             <td>"Portamento CC Control"</td>                            <td>"Controls the amount of Portamento."</td></tr>
 *  <tr><td>91</td>                                             <td>"Effect 1 Depth"</td>                                   <td>"Usually controls reverb send amount"</td></tr>
 *  <tr><td>92</td>                                             <td>"Effect 2 Depth"</td>                                   <td>"Usually controls tremolo amount"</td></tr>
 *  <tr><td>93</td>                                             <td>"Effect 3 Depth"</td>                                   <td>"Usually controls chorus amount"</td></tr>
 *  <tr><td>94</td>                                             <td>"Effect 4 Depth"</td>                                   <td>"Usually controls detune amount"</td></tr>
 *  <tr><td>95</td>                                             <td>"Effect 5 Depth"</td>                                   <td>"Usually controls phaser amount"</td></tr>
 *  <tr><td>96</td>                                             <td>"(+1) Data Increment"</td>                              <td>"Usually used to increment data for RPN and NRPN messages."</td></tr>
 *  <tr><td>97</td>                                             <td>"(-1) Data Decrement"</td>                              <td>"Usually used to decrement data for RPN and NRPN messages."</td></tr>
 *  <tr><td>98</td>                                             <td>"Non-Registered Parameter Number LSB (NRPN)"</td>       <td>"For controllers 6, 38, 96, and 97, it selects the NRPN parameter."</td></tr>
 *  <tr><td>99</td>                                             <td>"Non-Registered Parameter Number MSB (NRPN)"</td>       <td>"For controllers 6, 38, 96, and 97, it selects the NRPN parameter."</td></tr>
 *  <tr><td>100</td>                                            <td>"Registered Parameter Number LSB (RPN)"</td>            <td>"For controllers 6, 38, 96, and 97, it selects the RPN parameter."</td></tr>
 *  <tr><td>101</td>                                            <td>"Registered Parameter Number MSB (RPN)"</td>            <td>"For controllers 6, 38, 96, and 97, it selects the RPN parameter."</td></tr>
 *  <tr><td>3</td>                                              <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>9</td>                                              <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>14</td>                                             <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>15</td>                                             <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>16 – 19</td>                                        <td>General Purpose</td>                                    <td> </td></tr>
 *  <tr><td>20 – 31</td>                                        <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>32 – 63</td>                                        <td>Controller 0-31 Least Significant Bit (LSB)</td>        <td> </td></tr>
 *  <tr><td>85 – 90</td>                                        <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>102 – 119</td>                                      <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td colspan="3">120 to 127 are “Channel Mode Messages.”</td></tr>
 *  <tr><td>120</td>                                            <td>All Sound Off</td>                                      <td>Mutes all sounding notes. It does so regardless of release time or sustain. (See MIDI CC 123)</td></tr>
 *  <tr><td>121</td>                                            <td>Reset All Controllers</td>                              <td>It will reset all controllers to their default.</td></tr>
 *  <tr><td>122</td>                                            <td>Local On/Off Switch</td>                                <td>Turns internal connection of a MIDI keyboard/workstation, etc. On or Off. If you use a computer, you will most likely want local control off to avoid notes being played twice. Once locally and twice whent the note is sent back from the computer to your keyboard.</td></tr>
 *  <tr><td>123</td>                                            <td>All Notes Off</td>                                      <td>Mutes all sounding notes. Release time will still be maintained, and notes held by sustain will not turn off until sustain pedal is depressed.</td></tr>
 *  <tr><td>124</td>                                            <td>Omni Mode Off</td>                                      <td>Sets to “Omni Off” mode.</td></tr>
 *  <tr><td>125</td>                                            <td>Omni Mode On</td>                                       <td>Sets to “Omni On” mode.</td></tr>
 *  <tr><td>126</td>                                            <td>Mono Mode</td>                                          <td>Sets device mode to Monophonic.</td></tr>
 *  <tr><td>127</td>                                            <td>Poly Mode</td>                                          <td>Sets device mode to Polyphonic.</td></tr>
 *  </table>
 *   
 * @author Ats Oka
 *
 */

public class MetroMidiMessage implements MetroMidiReceiver<byte[]> {
	public static MetroMidiMessage getInstance() {
		return INSTANCE;
	}
	private static final MetroMidiMessage INSTANCE = new MetroMidiMessage();
	private MetroMidiMessage() {
	}
	
	/** @formatter:on */
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
	static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
	static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
	
	public byte[] noteOn( int channel, int note, double velocity ) {
		return MetroMidiMessageGen.noteOn (channel, note, velocity );
	}
	public byte[] noteOn( int channel, int note, int velocity ) {
		return MetroMidiMessageGen.noteOn (channel, note, velocity );
	}
	public byte[] noteOff ( int channel, int note, double velocity ) {
		return MetroMidiMessageGen.noteOff(channel, note, velocity );
	}
	public byte[] noteOff ( int channel, int note, int velocity ) {
		return MetroMidiMessageGen.noteOff(channel, note, velocity );
	}
	public byte[] keyPressure( int channel, int note, double value ) {
		return MetroMidiMessageGen.keyPressure( channel, note, value );
	}
	public byte[] keyPressure( int channel, int note, int value ) {
		return MetroMidiMessageGen.keyPressure( channel, note, value );
	}
	public byte[] controlChange( int channel, int controlNumber, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, controlNumber, controlValue );
	}
	public byte[] programChange( int channel, int value ) {
		return MetroMidiMessageGen.programChange( channel, value );
	}
	public byte[] channelPressure( int channel, double value ) {
		return MetroMidiMessageGen.channelPressure( channel, value );
	}
	public byte[] channelPressure( int channel, int value ) {
		return MetroMidiMessageGen.channelPressure( channel, value );
	}
	public byte[] pitchBend(int channel, double value) {
		return MetroMidiMessageGen.pitchBend( channel, value );
	}
	public byte[] pitchBend(int channel, int value) {
		return MetroMidiMessageGen.pitchBend( channel, value );
	}

	/*
	 * Channel Mode Control Change 
	 */
	public static final int CC_ALL_SOUND_OFF         = 120;
	public static final int CC_RESET_ALL_CONTROLLERS = 121;
	public static final int CC_LOCAL_CONTROLS        = 122;
	public static final int CC_ALL_NOTE_OFF          = 123;
	public static final int CC_OMNI_MODE_OFF         = 124;
	public static final int CC_OMNI_MODE_ON          = 125;
	public static final int CC_MONO_MODE_ON          = 126;
	public static final int CC_POLY_MODE_ON          = 127;

	public byte[] cc_allSoundOff(int channel) {
		return MetroMidiMessageGen.cc_allSoundOff( channel );
	}
	public byte[] cc_resetAllControllers(int channel) {
		return MetroMidiMessageGen.cc_resetAllControllers( channel );
	}
	public byte[] cc_localControls(int channel,boolean value ) {
		return MetroMidiMessageGen.cc_localControls( channel, value ) ;
	}
	public byte[] cc_allNoteOff(int channel) {
		return MetroMidiMessageGen.cc_allNoteOff( channel );
	}
	public byte[] cc_omniModeOff(int channel) {
		return MetroMidiMessageGen.cc_omniModeOff( channel );
	}
	public byte[] cc_omniModeOn(int channel) {
		return MetroMidiMessageGen.cc_omniModeOn( channel );
	}
	public byte[] cc_monoModeOn(int channel) {
		return MetroMidiMessageGen.cc_monoModeOn( channel );
	}
	public byte[] cc_polyModeOn(int channel) {
		return MetroMidiMessageGen.cc_polyModeOn( channel );
	}
	
	public byte[] songPositionPointer( int value ) {
		return MetroMidiMessageGen.songPositionPointer( value );
	}
	public byte[] songSelect(int value) {
		return MetroMidiMessageGen.songSelect( value );
	}
	public byte[] endOfExclusive () {
		return MetroMidiMessageGen.endOfExclusive();
	}
	public byte[] clock ( ) {
		return MetroMidiMessageGen.clock();
	}
	public byte[] start ( ) {
		return MetroMidiMessageGen.start();
	}
	public byte[] cont( ) {
		return MetroMidiMessageGen.cont();
	}
	public byte[] stop ( ) {
		return MetroMidiMessageGen.stop();
	}
	public byte[] reset ( ) {
		return MetroMidiMessageGen.reset();
	}
	
	
	public static final int CC_BANK_SELECT                            = 0  ;
	public byte[] cc_bankSelect( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_BANK_SELECT, controlValue );
	}
	public static final int CC_MODULATION                             = 1  ;
	public byte[] cc_modulation( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_MODULATION, controlValue );
	}
	public static final int CC_BREATH_CTRL                            = 2  ;
	public byte[] cc_breathController( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_BREATH_CTRL, controlValue );
	}
	public static final int CC_FOOT_CTRL                              = 4  ;
	public byte[] cc_footController( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_FOOT_CTRL, controlValue );
	}
	public static final int CC_PORTAMENTO_TIME                        = 5  ;
	public byte[] cc_portamentoTime( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_PORTAMENTO_TIME, controlValue );
	}
	public static final int CC_DATA_ENTRY_MSB                         = 6  ;
	public byte[] cc_dataEntryMsb( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_DATA_ENTRY_MSB, controlValue );
	}
	public static final int CC_VOLUME                                 = 7  ;
	public byte[] cc_volume( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_VOLUME, controlValue );
	}
	public static final int CC_BALANCE                                = 8  ;
	public byte[] cc_balance( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_BALANCE, controlValue );
	}
	public static final int CC_PAN                                    = 10 ;
	public byte[] cc_pan( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_PAN, controlValue );
	}
	public static final int CC_EXPRESSION                             = 11 ;
	public byte[] cc_expression( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EXPRESSION, controlValue );
	}
	public static final int CC_EFFECT_CTRL_1                          = 12 ;
	public byte[] cc_effectController1( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EFFECT_CTRL_1, controlValue );
	}
	public static final int CC_EFFECT_CTRL_2                          = 13 ;
	public byte[] cc_effectController2( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EFFECT_CTRL_2, controlValue );
	}
	public static final int CC_SUSTAIN_PEDAL                          = 64 ;
	public byte[] cc_sustainPedal( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SUSTAIN_PEDAL, controlValue );
	}
	public static final int CC_PORTAMENTO_SWITCH                      = 65 ;
	public byte[] cc_portamentoSwitch( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_PORTAMENTO_SWITCH, controlValue );
	}
	public static final int CC_SOSTENUTO_SWITCH                       = 66 ;
	public byte[] cc_sostenutoSwitch( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOSTENUTO_SWITCH, controlValue );
	}
	public static final int CC_SOFT_PEDAL_SWITCH                      = 67 ;
	public byte[] cc_pedalSwitch( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOFT_PEDAL_SWITCH, controlValue );
	}
	public static final int CC_LEGATO_FOOTSWITCH                      = 68 ;
	public byte[] cc_legatoSwitch( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_LEGATO_FOOTSWITCH, controlValue );
	}
	public static final int CC_HOLD_2                                 = 69 ;
	public byte[] cc_hold2( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_HOLD_2, controlValue );
	}
	public static final int CC_SOUND_CTRL_01                          = 70 ;
	public byte[] cc_soundController1( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_01, controlValue );
	}
	public static final int CC_SOUND_CTRL_02                          = 71 ;
	public byte[] cc_soundController2( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_02, controlValue );
	}
	public static final int CC_SOUND_CTRL_03                          = 72 ;
	public byte[] cc_soundController3( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_03, controlValue );
	}
	public static final int CC_SOUND_CTRL_04                          = 73 ;
	public byte[] cc_soundController4( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_04, controlValue );
	}
	public static final int CC_SOUND_CTRL_05                          = 74 ;
	public byte[] cc_soundController5( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_05, controlValue );
	}
	public static final int CC_SOUND_CTRL_06                          = 75 ;
	public byte[] cc_soundController6( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_06, controlValue );
	}
	public static final int CC_SOUND_CTRL_07                          = 76 ;
	public byte[] cc_soundController7( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_07, controlValue );
	}
	public static final int CC_SOUND_CTRL_08                          = 77 ;
	public byte[] cc_soundController8( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_08, controlValue );
	}
	public static final int CC_SOUND_CTRL_09                          = 78 ;
	public byte[] cc_soundController9( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_09, controlValue );
	}
	public static final int CC_SOUND_CTRL_10                          = 79 ;
	public byte[] cc_soundController10( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_SOUND_CTRL_10, controlValue );
	}
	public static final int CC_GENERAL_PURPOSE_01                     = 80 ;
	public byte[] cc_generalPurpose01( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_GENERAL_PURPOSE_01, controlValue );
	}
	public static final int CC_GENERAL_PURPOSE_02                     = 81 ;
	public byte[] cc_generalPurpose02( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_GENERAL_PURPOSE_02, controlValue );
	}
	public static final int CC_GENERAL_PURPOSE_03                     = 82 ;
	public byte[] cc_generalPurpose03( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_GENERAL_PURPOSE_03, controlValue );
	}
	public static final int CC_GENERAL_PURPOSE_04                     = 83 ;
	public byte[] cc_generalPurpose04( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_GENERAL_PURPOSE_04, controlValue );
	}
	public static final int CC_PORTAMENTO_CC_CTRL                     = 84 ;
	public byte[] cc_portamento( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_PORTAMENTO_CC_CTRL, controlValue );
	}
	public static final int CC_EFFECT_1_DEPTH                         = 91 ;
	public byte[] cc_effect1( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EFFECT_1_DEPTH, controlValue );
	}
	public static final int CC_EFFECT_2_DEPTH                         = 92 ;
	public byte[] cc_effect2( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EFFECT_2_DEPTH, controlValue );
	}
	public static final int CC_EFFECT_3_DEPTH                         = 93 ;
	public byte[] cc_effect3( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EFFECT_3_DEPTH, controlValue );
	}
	public static final int CC_EFFECT_4_DEPTH                         = 94 ;
	public byte[] cc_effect4( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EFFECT_4_DEPTH, controlValue );
	}
	public static final int CC_EFFECT_5_DEPTH                         = 95 ;
	public byte[] cc_effect5( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_EFFECT_5_DEPTH, controlValue );
	}
	public static final int CC_DATA_INCREMENT                         = 96 ;
	public byte[] cc_dataIncrement( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_DATA_INCREMENT, controlValue );
	}
	public static final int CC_DATA_DECREMENT                         = 97 ;
	public byte[] cc_dataDecrement( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_DATA_DECREMENT, controlValue );
	}
	public static final int CC_NRPN_LSB                               = 98 ;
	public byte[] cc_nrpnLsb( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_NRPN_LSB, controlValue );
	}
	public static final int CC_NRPN_MSB                               = 99 ;
	public byte[] cc_nrpnMsb( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_NRPN_MSB, controlValue );
	}
	public static final int CC_RPN_LSB                                = 100;
	public byte[] cc_rpnLsb( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_RPN_LSB, controlValue );
	}
	public static final int CC_RPN_MSB                                = 101;
	public byte[] cc_rpnMsb( int channel, int controlValue ) {
		return MetroMidiMessageGen.controlChange( channel, CC_RPN_MSB, controlValue );
	}
	@Override
	public byte[] error(String string) {
		return null;
	}
}

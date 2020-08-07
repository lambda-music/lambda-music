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

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.logging.Logger;

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
 *  <tr><td>66</td>                                             <td>"Sostenuto On/Off Switch"</td>                          <td>"On/Off switch - Like the Sustain controller (CC 64), However it only holds notes that were "On" when the pedal was pressed. People use it to "hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>67</td>                                             <td>"Soft Pedal On/Off Switch"</td>                         <td>"On/Off switch- Lowers the volume of notes played.0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>68</td>                                             <td>"Legato FootSwitch"</td>                                <td>"On/Off switch- Turns Legato effect between 2 subsequent notes On or Off.0 to 63 = Off, 64 to 127 = On"</td></tr>
 *  <tr><td>69</td>                                             <td>"Hold 2"</td>                                           <td>"Another way to "hold notes" (see MIDI CC 64 and MIDI CC 66). However notes fade out according to their release parameter rather than when the pedal is released."</td></tr>
 *  <tr><td>70</td>                                             <td>"Sound Controller 1"</td>                               <td>"Usually controls the way a sound is produced. Default = Sound Variation."</td></tr>
 *  <tr><td>71</td>                                             <td>"Sound Controller 2"</td>                               <td>"Allows shaping the Voltage Controlled Filter (VCF). Default = Resonance -also(Timbre or Harmonics)"</td></tr>
 *  <tr><td>72</td>                                             <td>"Sound Controller 3"</td>                               <td>"Controls release time of the Voltage controlled Amplifier (VCA). Default = Release Time."</td></tr>
 *  <tr><td>73</td>                                             <td>"Sound Controller 4"</td>                               <td>"Controls the "Attack" of a sound. The attack is the amount of time it takes forthe sound to reach maximum amplitude."</td></tr>
 *  <tr><td>74</td>                                             <td>"Sound Controller 5"</td>                               <td>"Controls VCFs cutoff frequency of the filter."</td></tr>
 *  <tr><td>75</td>                                             <td>"Sound Controller 6"</td>                               <td>"Generic - Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>76</td>                                             <td>"Sound Controller 7"</td>                               <td>"Generic - Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>77</td>                                             <td>"Sound Controller 8"</td>                               <td>"Generic - Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>78</td>                                             <td>"Sound Controller 9"</td>                               <td>"Generic - Some manufacturers may use to further shave their sounds."</td></tr>
 *  <tr><td>79</td>                                             <td>"Sound Controller 10"</td>                              <td>"Generic - Some manufacturers may use to further shave their sounds."</td></tr>
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
 *  <tr><td>16 - 19</td>                                        <td>General Purpose</td>                                    <td> </td></tr>
 *  <tr><td>20 - 31</td>                                        <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>32 - 63</td>                                        <td>Controller 0-31 Least Significant Bit (LSB)</td>        <td> </td></tr>
 *  <tr><td>85 - 90</td>                                        <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td>102 - 119</td>                                      <td>Undefined</td>                                          <td> </td></tr>
 *  <tr><td colspan="3">120 to 127 are "Channel Mode Messages."</td></tr>
 *  <tr><td>120</td>                                            <td>All Sound Off</td>                                      <td>Mutes all sounding notes. It does so regardless of release time or sustain. (See MIDI CC 123)</td></tr>
 *  <tr><td>121</td>                                            <td>Reset All Controllers</td>                              <td>It will reset all controllers to their default.</td></tr>
 *  <tr><td>122</td>                                            <td>Local On/Off Switch</td>                                <td>Turns internal connection of a MIDI keyboard/workstation, etc. On or Off. If you use a computer, you will most likely want local control off to avoid notes being played twice. Once locally and twice whent the note is sent back from the computer to your keyboard.</td></tr>
 *  <tr><td>123</td>                                            <td>All Notes Off</td>                                      <td>Mutes all sounding notes. Release time will still be maintained, and notes held by sustain will not turn off until sustain pedal is depressed.</td></tr>
 *  <tr><td>124</td>                                            <td>Omni Mode Off</td>                                      <td>Sets to "Omni Off" mode.</td></tr>
 *  <tr><td>125</td>                                            <td>Omni Mode On</td>                                       <td>Sets to "Omni On" mode.</td></tr>
 *  <tr><td>126</td>                                            <td>Mono Mode</td>                                          <td>Sets device mode to Monophonic.</td></tr>
 *  <tr><td>127</td>                                            <td>Poly Mode</td>                                          <td>Sets device mode to Polyphonic.</td></tr>
 *  </table>
 *   
 * @author Ats Oka
 *
 */

public class MetroMidiMessageReceiver implements MetroMidiReceiver<byte[]> {
    public static MetroMidiMessageReceiver getInstance() {
        return INSTANCE;
    }
    private static final MetroMidiMessageReceiver INSTANCE = new MetroMidiMessageReceiver();
    protected MetroMidiMessageReceiver() {
    }
    
    /** @formatter:on */
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    // system
    private boolean endCalled=false;
    @Override
    public boolean endCalled() {
        return endCalled;
    }
    @Override
    public void end(MetroCollector<byte[]> result) {
        this.endCalled = true;
        result.collect( voidValue( "end" ) );
    }
    @Override
    public void error(MetroCollector<byte[]> result, String string) {
        result.collect( voidValue( string ));
    }
    
    // basic
    public void noteOn(MetroCollector<byte[]> result,  int channel, int note, double velocity ) {
        result.collect(MetroMidiMessages.noteOn (channel, note, velocity ));
    }
    public void noteOn(MetroCollector<byte[]> result,  int channel, int note, int velocity ) {
        result.collect(MetroMidiMessages.noteOn (channel, note, velocity ));
    }
    public void noteOff (MetroCollector<byte[]> result,  int channel, int note, double velocity ) {
        result.collect(MetroMidiMessages.noteOff(channel, note, velocity ));
    }
    public void noteOff (MetroCollector<byte[]> result,  int channel, int note, int velocity ) {
        result.collect(MetroMidiMessages.noteOff(channel, note, velocity ));
    }
    public void keyPressure(MetroCollector<byte[]> result,  int channel, int note, double value ) {
        result.collect(MetroMidiMessages.keyPressure( channel, note, value ));
    }
    public void keyPressure(MetroCollector<byte[]> result,  int channel, int note, int value ) {
        result.collect(MetroMidiMessages.keyPressure( channel, note, value ));
    }
    public void controlChange(MetroCollector<byte[]> result,  int channel, int controlNumber, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, controlNumber, controlValue ));
    }
    public void programChange(MetroCollector<byte[]> result,  int channel, int value ) {
        result.collect(MetroMidiMessages.programChange( channel, value ));
    }
    public void channelPressure(MetroCollector<byte[]> result,  int channel, double value ) {
        result.collect(MetroMidiMessages.channelPressure( channel, value ));
    }
    public void channelPressure(MetroCollector<byte[]> result,  int channel, int value ) {
        result.collect(MetroMidiMessages.channelPressure( channel, value ));
    }
    public void pitchBend(MetroCollector<byte[]> result, int channel, double value) {
        result.collect(MetroMidiMessages.pitchBend( channel, value ));
    }
    public void pitchBend(MetroCollector<byte[]> result, int channel, int value) {
        result.collect(MetroMidiMessages.pitchBend( channel, value ));
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

    public void cc_allSoundOff(MetroCollector<byte[]> result, int channel) {
        result.collect(MetroMidiMessages.cc_allSoundOff( channel ));
    }
    public void cc_resetAllControllers(MetroCollector<byte[]> result, int channel) {
        result.collect(MetroMidiMessages.cc_resetAllControllers( channel ));
    }
    public void cc_localControls(MetroCollector<byte[]> result, int channel,boolean value ) {
        result.collect(MetroMidiMessages.cc_localControls( channel, value )) ;
    }
    public void cc_allNoteOff(MetroCollector<byte[]> result, int channel) {
        result.collect(MetroMidiMessages.cc_allNoteOff( channel ));
    }
    public void cc_omniModeOff(MetroCollector<byte[]> result, int channel) {
        result.collect(MetroMidiMessages.cc_omniModeOff( channel ));
    }
    public void cc_omniModeOn(MetroCollector<byte[]> result, int channel) {
        result.collect(MetroMidiMessages.cc_omniModeOn( channel ));
    }
    public void cc_monoModeOn(MetroCollector<byte[]> result, int channel) {
        result.collect(MetroMidiMessages.cc_monoModeOn( channel ));
    }
    public void cc_polyModeOn(MetroCollector<byte[]> result, int channel) {
        result.collect(MetroMidiMessages.cc_polyModeOn( channel ));
    }
    
    public void songPositionPointer(MetroCollector<byte[]> result,  int value ) {
        result.collect(MetroMidiMessages.songPositionPointer( value ));
    }
    public void songSelect(MetroCollector<byte[]> result, int value) {
        result.collect(MetroMidiMessages.songSelect( value ));
    }
    public void endOfExclusive (MetroCollector<byte[]> result) {
        result.collect(MetroMidiMessages.endOfExclusive());
    }
    public void clock (MetroCollector<byte[]> result) {
        result.collect(MetroMidiMessages.clock());
    }
    public void start (MetroCollector<byte[]> result) {
        result.collect(MetroMidiMessages.start());
    }
    public void cont(MetroCollector<byte[]> result) {
        result.collect(MetroMidiMessages.cont());
    }
    public void stop (MetroCollector<byte[]> result) {
        result.collect(MetroMidiMessages.stop());
    }
    public void reset (MetroCollector<byte[]> result) {
        result.collect(MetroMidiMessages.reset());
    }
    
    
    public static final int CC_BANK_SELECT                            = 0  ;
    public void cc_bankSelect(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_BANK_SELECT, controlValue ));
    }
    public static final int CC_MODULATION                             = 1  ;
    public void cc_modulation(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_MODULATION, controlValue ));
    }
    public static final int CC_BREATH_CTRL                            = 2  ;
    public void cc_breathController(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_BREATH_CTRL, controlValue ));
    }
    public static final int CC_FOOT_CTRL                              = 4  ;
    public void cc_footController(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_FOOT_CTRL, controlValue ));
    }
    public static final int CC_PORTAMENTO_TIME                        = 5  ;
    public void cc_portamentoTime(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_PORTAMENTO_TIME, controlValue ));
    }
    public static final int CC_DATA_ENTRY_MSB                         = 6  ;
    public void cc_dataEntryMsb(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_DATA_ENTRY_MSB, controlValue ));
    }
    public static final int CC_VOLUME                                 = 7  ;
    public void cc_volume(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_VOLUME, controlValue ));
    }
    public static final int CC_BALANCE                                = 8  ;
    public void cc_balance(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_BALANCE, controlValue ));
    }
    public static final int CC_PAN                                    = 10 ;
    public void cc_pan(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_PAN, controlValue ));
    }
    public static final int CC_EXPRESSION                             = 11 ;
    public void cc_expression(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EXPRESSION, controlValue ));
    }
    public static final int CC_EFFECT_CTRL_1                          = 12 ;
    public void cc_effectController1(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EFFECT_CTRL_1, controlValue ));
    }
    public static final int CC_EFFECT_CTRL_2                          = 13 ;
    public void cc_effectController2(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EFFECT_CTRL_2, controlValue ));
    }
    public static final int CC_SUSTAIN_PEDAL                          = 64 ;
    public void cc_sustainPedal(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SUSTAIN_PEDAL, controlValue ));
    }
    public static final int CC_PORTAMENTO_SWITCH                      = 65 ;
    public void cc_portamentoSwitch(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_PORTAMENTO_SWITCH, controlValue ));
    }
    public static final int CC_SOSTENUTO_SWITCH                       = 66 ;
    public void cc_sostenutoSwitch(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOSTENUTO_SWITCH, controlValue ));
    }
    public static final int CC_SOFT_PEDAL_SWITCH                      = 67 ;
    public void cc_pedalSwitch(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOFT_PEDAL_SWITCH, controlValue ));
    }
    public static final int CC_LEGATO_FOOTSWITCH                      = 68 ;
    public void cc_legatoSwitch(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_LEGATO_FOOTSWITCH, controlValue ));
    }
    public static final int CC_HOLD_2                                 = 69 ;
    public void cc_hold2(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_HOLD_2, controlValue ));
    }
    public static final int CC_SOUND_CTRL_01                          = 70 ;
    public void cc_soundController1(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_01, controlValue ));
    }
    public static final int CC_SOUND_CTRL_02                          = 71 ;
    public void cc_soundController2(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_02, controlValue ));
    }
    public static final int CC_SOUND_CTRL_03                          = 72 ;
    public void cc_soundController3(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_03, controlValue ));
    }
    public static final int CC_SOUND_CTRL_04                          = 73 ;
    public void cc_soundController4(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_04, controlValue ));
    }
    public static final int CC_SOUND_CTRL_05                          = 74 ;
    public void cc_soundController5(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_05, controlValue ));
    }
    public static final int CC_SOUND_CTRL_06                          = 75 ;
    public void cc_soundController6(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_06, controlValue ));
    }
    public static final int CC_SOUND_CTRL_07                          = 76 ;
    public void cc_soundController7(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_07, controlValue ));
    }
    public static final int CC_SOUND_CTRL_08                          = 77 ;
    public void cc_soundController8(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_08, controlValue ));
    }
    public static final int CC_SOUND_CTRL_09                          = 78 ;
    public void cc_soundController9(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_09, controlValue ));
    }
    public static final int CC_SOUND_CTRL_10                          = 79 ;
    public void cc_soundController10(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_SOUND_CTRL_10, controlValue ));
    }
    public static final int CC_GENERAL_PURPOSE_01                     = 80 ;
    public void cc_generalPurpose01(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_GENERAL_PURPOSE_01, controlValue ));
    }
    public static final int CC_GENERAL_PURPOSE_02                     = 81 ;
    public void cc_generalPurpose02(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_GENERAL_PURPOSE_02, controlValue ));
    }
    public static final int CC_GENERAL_PURPOSE_03                     = 82 ;
    public void cc_generalPurpose03(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_GENERAL_PURPOSE_03, controlValue ));
    }
    public static final int CC_GENERAL_PURPOSE_04                     = 83 ;
    public void cc_generalPurpose04(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_GENERAL_PURPOSE_04, controlValue ));
    }
    public static final int CC_PORTAMENTO_CC_CTRL                     = 84 ;
    public void cc_portamento(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_PORTAMENTO_CC_CTRL, controlValue ));
    }
    public static final int CC_EFFECT_1_DEPTH                         = 91 ;
    public void cc_effect1(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EFFECT_1_DEPTH, controlValue ));
    }
    public static final int CC_EFFECT_2_DEPTH                         = 92 ;
    public void cc_effect2(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EFFECT_2_DEPTH, controlValue ));
    }
    public static final int CC_EFFECT_3_DEPTH                         = 93 ;
    public void cc_effect3(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EFFECT_3_DEPTH, controlValue ));
    }
    public static final int CC_EFFECT_4_DEPTH                         = 94 ;
    public void cc_effect4(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EFFECT_4_DEPTH, controlValue ));
    }
    public static final int CC_EFFECT_5_DEPTH                         = 95 ;
    public void cc_effect5(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_EFFECT_5_DEPTH, controlValue ));
    }
    public static final int CC_DATA_INCREMENT                         = 96 ;
    public void cc_dataIncrement(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_DATA_INCREMENT, controlValue ));
    }
    public static final int CC_DATA_DECREMENT                         = 97 ;
    public void cc_dataDecrement(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_DATA_DECREMENT, controlValue ));
    }
    public static final int CC_NRPN_LSB                               = 98 ;
    public void cc_nrpnLsb(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_NRPN_LSB, controlValue ));
    }
    public static final int CC_NRPN_MSB                               = 99 ;
    public void cc_nrpnMsb(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_NRPN_MSB, controlValue ));
    }
    public static final int CC_RPN_LSB                                = 100;
    public void cc_rpnLsb(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_RPN_LSB, controlValue ));
    }
    public static final int CC_RPN_MSB                                = 101;
    public void cc_rpnMsb(MetroCollector<byte[]> result,  int channel, int controlValue ) {
        result.collect(MetroMidiMessages.controlChange( channel, CC_RPN_MSB, controlValue ));
    }
}

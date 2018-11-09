package ats.metro;

import java.util.HashMap;
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
 *  <tr><td>6</td>                                              <td>"Data Entry Most Significant Bit(MSB)"</td>             <td>"Controls Value for NRPN or RPN parameters."</td></tr>
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

public class MetroMidi {
	/** @formatter:on */

	static final Logger LOGGER = Logger.getLogger( MetroMidi.class.getName() );

	static HashMap<String,MetroMidiMsg> infoMap = new HashMap<String,MetroMidiMsg>();
	static void putInfo( MetroMidiMsg info ) {
		String id = info.shortName;
		if ( infoMap.containsKey(id))
			throw new RuntimeException( "internal error : id (" + id + ") is already registered." );
		
		infoMap.put( id, info );
	}
	public static MetroMidiMsg getInfo( String id ) {
		return infoMap.get(id);
	}

	public static abstract class MetroMidiMsg {
		protected String shortName;
		protected String longName;
		protected String shortDescription;
		protected String longDescription;
		public String getShortName() {
			return shortName;
		}		
		public String getLongName() {
			return longName;
		}
		public String getShortDescription() {
			return shortDescription;
		}
		public String getLongDescription() {
			return longDescription;
		}
	}
	public static abstract class MetroMidiNoArg extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int ch );
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch ) {
			buf.midiEvent(offset , port, createMidiMessage( ch ) );
		}
	}
	public static abstract class MetroMidiBoolean1 extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int ch, boolean value );
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, boolean value ) {
			buf.midiEvent(offset , port, createMidiMessage( ch, value ) );
		}
	}
	public static abstract class MetroMidiInt1 extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int ch, int value );
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, int value  ) {
			buf.midiEvent(offset , port, createMidiMessage( ch, value ) );
		}
	}
	public static abstract class MetroMidiInt2 extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int ch, int value0, int value1 );
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, int value0, int value1  ) {
			buf.midiEvent(offset , port, createMidiMessage( ch, value0, value1 ) );
		}
	}
	public static abstract class MetroMidiDouble1 extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int ch, double value );
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, double value  ) {
			buf.midiEvent(offset , port, createMidiMessage( ch, value ) );
		}
	}
	
	public static abstract class MetroMidiControlChangeBase extends MetroMidiMsg {
		int controlNumber;
		public final byte[] createMidiMessage( int ch, int value ) {
			return MetroMidiMessage.controlChange( ch, controlNumber, value );
		}
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, int value  ) {
			buf.midiEvent(offset , port, createMidiMessage( ch, value ) );
		}
	}
	public static abstract class MetroMidiNoChannelNoArg extends MetroMidiMsg {
		public abstract byte[] createMidiMessage();
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port ) {
			buf.midiEvent(offset , port, createMidiMessage() );
		}
	}
	public static abstract class MetroMidiNoChannelInt1 extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int value );
		public final void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int value ) {
			buf.midiEvent(offset , port, createMidiMessage( value ) );
		}
	}
	public static abstract class MetroMidiInt1Double1 extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int ch, int value0, double value1 );
//		public abstract <T> T execute(MetroMidiReceiver<T> receiver, int ch, int note0, double value1);
		public void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, int note0, double value1 ) {
			buf.midiEvent( offset, port, createMidiMessage( ch, note0, value1 ) );
		}
	}
	public static abstract class MetroMidiInt1Double2 extends MetroMidiMsg {
		public abstract byte[] createMidiMessage( int ch, int value0, double value1, double value2 );
		public void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, int note0, double value1, double value2 ) {
			buf.midiEvent( offset, port, createMidiMessage( ch, note0, value1, value2 ) );
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////////
	
	public static abstract class MetroMidiNote extends MetroMidiInt1Double1 {
	}

	//////////////////////////////

	public static final MetroMidiNoteOn MIDI_NOTE_ON = new MetroMidiNoteOn();
	public static final class MetroMidiNoteOn extends MetroMidiNote {
		{
			this.shortName = "non";
			this.longName = "note on";
		}
		@Override
		public byte[] createMidiMessage( int ch, int note, double velocity ) {
			return MetroMidiMessage.noteOn (ch, note, velocity );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int ch, int note, double velocity ) {
			return receiver.noteOn( ch, note, velocity ); 
		}
	}
	public static final MetroMidiNoteOff MIDI_NOTE_OFF = new MetroMidiNoteOff();
	public static final class MetroMidiNoteOff extends MetroMidiNote {
		{
			this.shortName = "noff";
			this.longName = "note off";
		}
		@Override
		public byte[] createMidiMessage( int ch, int note, double velocity ) {
			return MetroMidiMessage.noteOff(ch, note, velocity );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int ch, int note, double velocity ) {
			return receiver.noteOff (ch, note, velocity );
		}
	}
	
	
	public static final MetroMidiKeyPressure MIDI_KEY_PRESSURE = new MetroMidiKeyPressure();
	public static final class MetroMidiKeyPressure extends MetroMidiInt1Double1 {
		{
			this.shortName = "kp";
			this.longName  = "key-pressure";
		}

		@Override
		public byte[] createMidiMessage( int ch, int note, double value ) {
			return MetroMidiMessage.keyPressure( ch, note, value );
		}

		public <T> T execute( MetroMidiReceiver<T> receiver, int ch, int note, double value ) {
			return receiver.keyPressure ( ch, note, value );
		}
	}
	public static final MetroMidiControlChange MIDI_CONTROL_CHANGE = new MetroMidiControlChange();
	public static final class MetroMidiControlChange extends MetroMidiMsg {
		{
			this.shortName = "cc";
			// this.name = "control";
			this.longName = "control-change";
		}
		public byte[] createMidiMessage( int ch, int controlNumber, int controlValue ) {
			return MetroMidiMessage.controlChange(ch, controlNumber, controlValue );
		}
		public void notifyMidiEvent( MetroEventBuffer buf, double offset, int port, int ch, int controlNumber, int controlValue ) {
			buf.midiEvent(offset, port, createMidiMessage( ch, controlNumber, controlValue ) );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int ch, int controlNumber, int controlValue ) {
			return receiver.controlChange( ch, controlNumber, controlValue );
		}
	}
	public static final MetroMidiProgramChange  MIDI_PROGRAM_CHANGE = new MetroMidiProgramChange(); 
	public static final class MetroMidiProgramChange extends MetroMidiInt1 {
		{
			this.shortName = "pc";
			this.longName = "program";
		}
		@Override
		public byte[] createMidiMessage( int ch, int value ) {
			return MetroMidiMessage.programChange( ch, value );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int ch, int value ) {
			return receiver.programChange ( ch, value );
		}
	}
	public static final MetroMidiChannelPressure  MIDI_CHANNEL_PRESSURE = new MetroMidiChannelPressure(); 
	public static final class MetroMidiChannelPressure extends MetroMidiDouble1 {
		{
			this.shortName = "cp";
			this.longName = "channel-pressure";
		}
		@Override
		public byte[] createMidiMessage( int ch, double value ) {
			return MetroMidiMessage.channelPressure( ch, value );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver , int ch, double value ) {
			return receiver.channelPressure ( ch, value );
		}
	}
	public static final MetroMidiPitchBend  MIDI_PITCH_BEND = new MetroMidiPitchBend(); 
	public static final class MetroMidiPitchBend extends MetroMidiDouble1 {
		{
			this.shortName = "pb";
			this.longName = "pitch-bend";
		}
		@Override
		public byte[] createMidiMessage(int ch, double value) {
			return MetroMidiMessage.pitchBend( ch, value );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch, double value) {
			return receiver.pitchBend ( ch, value );
		}
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

	public static final MetroMidiAllSoundOff  MIDI_ALL_SOUND_OFF = new MetroMidiAllSoundOff(); 
	public static final class MetroMidiAllSoundOff extends MetroMidiNoArg {
		{
			this.shortName = "aso";
			this.longName = "all-sound-off";
		}
		@Override
		public byte[] createMidiMessage(int ch) {
			return MetroMidiMessage.cc_allSoundOff( ch );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch) {
			return receiver.cc_allSoundOff ( ch );
		}
	}
	public static final MetroMidiResetAllControllers  MIDI_RESET_ALL_CONTROLLERS = new MetroMidiResetAllControllers(); 
	public static final class MetroMidiResetAllControllers extends MetroMidiNoArg {
		{
			this.shortName = "rac";
			this.longName = "reset-all-controllers";
		}
		@Override
		public byte[] createMidiMessage(int ch) {
			return MetroMidiMessage.cc_resetAllControllers( ch );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch) {
			return receiver.cc_resetAllControllers ( ch );
		}
	}
	public static final MetroMidiLocalControls  MIDI_LOCAL_CONTROLS = new MetroMidiLocalControls(); 
	public static final class MetroMidiLocalControls extends MetroMidiBoolean1 {
		{
			this.shortName = "lc";
			this.longName = "local-controls";
		}
		@Override
		public byte[] createMidiMessage(int ch,boolean value ) {
			return MetroMidiMessage.cc_localControls( ch, value ) ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch,boolean value ) {
			return receiver.cc_localControls ( ch, value );
		}
	}
	public static final MetroMidiAllNoteOff  MIDI_ALL_NOTE_OFF = new MetroMidiAllNoteOff(); 
	public static final class MetroMidiAllNoteOff extends MetroMidiNoArg {
		{
			this.shortName = "anf";
			this.longName = "all-note-off";
		}
		@Override
		public byte[] createMidiMessage(int ch) {
			return MetroMidiMessage.cc_allNoteOff( ch );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch) {
			return receiver.cc_allNoteOff ( ch );
		}
	}
	public static final MetroMidiOmniModeOff  MIDI_OMNI_MODE_OFF = new MetroMidiOmniModeOff(); 
	public static final class MetroMidiOmniModeOff extends MetroMidiNoArg {
		{
			this.shortName = "omff";
			this.longName = "omni-mode-off";
		}
		@Override
		public byte[] createMidiMessage(int ch) {
			return MetroMidiMessage.cc_omniModeOff( ch );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch) {
			return receiver.cc_omniModeOff ( ch );
		}
		
	}
	public static final MetroMidiOmniModeOn  MIDI_OMNI_MODE_ON = new MetroMidiOmniModeOn(); 
	public static final class MetroMidiOmniModeOn extends MetroMidiNoArg {
		{
			this.shortName = "omon";
			this.longName = "omni-mode-on";
		}
		@Override
		public byte[] createMidiMessage(int ch) {
			return MetroMidiMessage.cc_omniModeOn( ch );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch) {
			return receiver.cc_omniModeOn ( ch );
		}
	}
	// TODO : Isn't this mono-mode-on? This seems incorrect.  
	public static final MetroMidiMonoModeOn  MIDI_MONO_MODE_ON = new MetroMidiMonoModeOn(); 
	public static final class MetroMidiMonoModeOn extends MetroMidiNoArg {
		{
			this.shortName = "mono";
			this.longName = "mono-mode-on";
		}
		@Override
		public byte[] createMidiMessage(int ch) {
			return MetroMidiMessage.cc_monoModeOn( ch );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch) {
			return receiver.cc_monoModeOn ( ch );
		}
	}
	public static final MetroMidiPolyModeOn  MIDI_POLY_MODE_ON = new MetroMidiPolyModeOn(); 
	public static final class MetroMidiPolyModeOn extends MetroMidiNoArg {
		{
			this.shortName = "poly";
			this.longName = "poly-mode-on";
		}
		@Override
		public byte[] createMidiMessage(int ch) {
			return MetroMidiMessage.cc_polyModeOn( ch );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int ch) {
			return receiver.cc_polyModeOn ( ch );
		}
	}
	
	// TODO : the channel value is not necessary. Remove it from SchemeNoteParser , too. 
	public static final MetroMidiSongPositionPointer  MIDI_SONG_POSITION_POINTER = new MetroMidiSongPositionPointer(); 
	public static final class MetroMidiSongPositionPointer extends MetroMidiNoChannelInt1 {
		{
			this.shortName = "spp";
			this.longName = "sys-song-position-pointer";
		}
		@Override
		public byte[] createMidiMessage( int value ) {
			return MetroMidiMessage.songPositionPointer( value );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver , int value ) {
			return receiver.songPositionPointer ( value );
		}
	}
	// TODO : the channel value is not necessary. Remove it from SchemeNoteParser , too. 
	public static final MetroMidiSongSelect  MIDI_SONG_SELECT = new MetroMidiSongSelect(); 
	public static final class MetroMidiSongSelect extends MetroMidiNoChannelInt1 {
		{
			this.shortName = "ss";
			this.longName = "sys-song-select";
		}
		@Override
		public byte[] createMidiMessage(int value) {
			return MetroMidiMessage.songSelect( value );
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ,int value) {
			return receiver.songSelect ( value );
		}
	}
	public static final MetroMidiEndOfExclusive  MIDI_END_OF_EXCLUSIVE = new MetroMidiEndOfExclusive(); 
	public static final class MetroMidiEndOfExclusive extends MetroMidiNoChannelNoArg {
		{
			this.shortName = "eoe";
			this.longName = "sys-end-of-exclusive";
		}
		@Override
		public byte[] createMidiMessage() {
			return MetroMidiMessage.endOfExclusive();
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ) {
			return receiver.endOfExclusive ();
		}
	}
	public static final MetroMidiClock  MIDI_CLOCK = new MetroMidiClock(); 
	public static final class MetroMidiClock extends MetroMidiNoChannelNoArg {
		{
			this.shortName = "clock";
			this.longName = "sys-clock";
		}
		@Override
		public byte[] createMidiMessage() {
			return MetroMidiMessage.clock();
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ) {
			return receiver.clock ();
		}
	}
	
	public static final MetroMidiStart  MIDI_START = new MetroMidiStart(); 
	public static final class MetroMidiStart extends MetroMidiNoChannelNoArg {
		{
			this.shortName = "start";
			this.longName = "sys-start";
		}
		@Override
		public byte[] createMidiMessage() {
			return MetroMidiMessage.start();
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ) {
			return receiver.start ();
		}
	}
	public static final MetroMidiContinue  MIDI_CONTINUE = new MetroMidiContinue(); 
	public static final class MetroMidiContinue extends MetroMidiNoChannelNoArg {
		{
			this.shortName = "cont";
			this.longName = "sys-continue";
		}
		@Override
		public byte[] createMidiMessage() {
			return MetroMidiMessage.cont();
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ) {
			return receiver.cont();
		}
	}
	public static final MetroMidiStop  MIDI_STOP = new MetroMidiStop(); 
	public static final class MetroMidiStop extends MetroMidiNoChannelNoArg {
		{
			this.shortName = "stop";
			this.longName = "sys-stop";
		}
		@Override
		public byte[] createMidiMessage() {
			return MetroMidiMessage.stop();
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ) {
			return receiver.stop ();
		}
	}
	public static final MetroMidiReset  MIDI_RESET = new MetroMidiReset(); 
	public static final class MetroMidiReset extends MetroMidiNoChannelNoArg {
		{
			this.shortName = "reset";
			this.longName = "sys-reset";
		}
		@Override
		public byte[] createMidiMessage() {
			return MetroMidiMessage.reset();
		}
		public <T> T execute( MetroMidiReceiver<T> receiver ) {
			return receiver.reset ();
		}
	}
	
	
	public static final int CC_BANK_SELECT                            = 0  ;
	public static final MetroMidiControlBankSelect MIDI_BANK_SELECT  = new MetroMidiControlBankSelect();
	public static final class MetroMidiControlBankSelect extends MetroMidiControlChangeBase {
		{
			this.shortName = "bs";
			this.longName = "bank-select";
			this.shortDescription = "Bank Select";
			this.longDescription = "Allows user to switch bank for patch selection. Program change used with Bank Select. MIDI can access 16,384 patches per MIDI channel.";
			this.controlNumber = CC_BANK_SELECT                            ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_bankSelect ( channel, value );
		}
	}
	public static final int CC_MODULATION                             = 1  ;
	public static final MetroMidiControlModulation MIDI_MODULATION  = new MetroMidiControlModulation();
	public static final class MetroMidiControlModulation extends MetroMidiControlChangeBase {
		{
			this.shortName = "mod";
			this.longName = "modulation";
			this.shortDescription = "Modulation";
			this.longDescription = "Generally this CC controls a vibrato effect (pitch, loudness, brighness). What is modulated is based on the patch.";
			this.controlNumber = CC_MODULATION                             ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_modulation ( channel, value );
		}
	}
	public static final int CC_BREATH_CTRL                            = 2  ;
	public static final MetroMidiControlBreathController MIDI_BREATH_CTRL  = new MetroMidiControlBreathController();
	public static final class MetroMidiControlBreathController extends MetroMidiControlChangeBase {
		{
			this.shortName = "bc";
			this.longName = "breath-controller";
			this.shortDescription = "Breath Controller";
			this.longDescription = "Often times associated with aftertouch messages. It was originally intended for use with a breath MIDI controller in which blowing harder produced higher MIDI control values. It can be used for modulation as well.";
			this.controlNumber = CC_BREATH_CTRL                            ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_breathController ( channel, value );
		}
	}
	public static final int CC_FOOT_CTRL                              = 4  ;
	public static final MetroMidiControlFootController MIDI_FOOT_CTRL  = new MetroMidiControlFootController();
	public static final class MetroMidiControlFootController extends MetroMidiControlChangeBase {
		{
			this.shortName = "fc";
			this.longName = "foot-controller";
			this.shortDescription = "Foot Controller";
			this.longDescription = "Often used with aftertouch messages. It can send a continuous stream of values based on how the pedal is used.";
			this.controlNumber = CC_FOOT_CTRL                              ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_footController ( channel, value );
		}
	}
	public static final int CC_PORTAMENTO_TIME                        = 5  ;
	public static final MetroMidiControlPortamentoTime MIDI_PORTAMENTO_TIME  = new MetroMidiControlPortamentoTime();
	public static final class MetroMidiControlPortamentoTime extends MetroMidiControlChangeBase {
		{
			this.shortName = "pt";
			this.longName = "portamento-time";
			this.shortDescription = "Portamento Time";
			this.longDescription = "Controls portamento rate to slide between 2 notes played subsequently.";
			this.controlNumber = CC_PORTAMENTO_TIME                        ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_portamentoTime ( channel, value );
		}
	}
	public static final int CC_DATA_ENTRY_MSB                         = 6  ;
	public static final MetroMidiControlDataEntryMsb MIDI_DATA_ENTRY_MSB  = new MetroMidiControlDataEntryMsb();
	public static final class MetroMidiControlDataEntryMsb extends MetroMidiControlChangeBase {
		{
			this.shortName = "de-msb";
			this.longName = "data-entry-msb";
			this.shortDescription = "Data Entry Most Significant Bit(MSB)";
			this.longDescription = "Controls Value for NRPN or RPN parameters.";
			this.controlNumber = CC_DATA_ENTRY_MSB                         ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_dataEntryMsb ( channel, value );
		}
	}
	public static final int CC_VOLUME                                 = 7  ;
	public static final MetroMidiControlVolume MIDI_VOLUME  = new MetroMidiControlVolume();
	public static final class MetroMidiControlVolume extends MetroMidiControlChangeBase {
		{
			this.shortName = "v";
			this.longName = "volume";
			this.shortDescription = "Volume";
			this.longDescription = "Control the volume of the channel";
			this.controlNumber = CC_VOLUME                                 ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_volume ( channel, value );
		}
	}
	public static final int CC_BALANCE                                = 8  ;
	public static final MetroMidiControlBalance MIDI_BALANCE  = new MetroMidiControlBalance();
	public static final class MetroMidiControlBalance extends MetroMidiControlChangeBase {
		{
			this.shortName = "b";
			this.longName = "balance";
			this.shortDescription = "Balance";
			this.longDescription = "Controls the left and right balance, generally for stereo patches.0 = hard left, 64 = center, 127 = hard right";
			this.controlNumber = CC_BALANCE                                ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_balance ( channel, value );
		}
	}
	public static final int CC_PAN                                    = 10 ;
	public static final MetroMidiControlPan MIDI_PAN  = new MetroMidiControlPan();
	public static final class MetroMidiControlPan extends MetroMidiControlChangeBase {
		{
			this.shortName = "p";
			this.longName = "pan";
			this.shortDescription = "Pan";
			this.longDescription = "Controls the left and right balance, generally for mono patches.0 = hard left, 64 = center, 127 = hard right";
			this.controlNumber = CC_PAN                                    ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_pan ( channel, value );
		}
	}
	public static final int CC_EXPRESSION                             = 11 ;
	public static final MetroMidiControlExpression MIDI_EXPRESSION  = new MetroMidiControlExpression();
	public static final class MetroMidiControlExpression extends MetroMidiControlChangeBase {
		{
			this.shortName = "e";
			this.longName = "expression";
			this.shortDescription = "Expression";
			this.longDescription = "Expression is a percentage of volume (CC7).";
			this.controlNumber = CC_EXPRESSION                             ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_expression ( channel, value );
		}
	}
	public static final int CC_EFFECT_CTRL_1                          = 12 ;
	public static final MetroMidiControlEffectController1 MIDI_EFFECT_CTRL_1  = new MetroMidiControlEffectController1();
	public static final class MetroMidiControlEffectController1 extends MetroMidiControlChangeBase {
		{
			this.shortName = "ec1";
			this.longName = "effect-controller-1";
			this.shortDescription = "Effect Controller 1";
			this.longDescription = "Usually used to control a parameter of an effect within the synth/workstation.";
			this.controlNumber = CC_EFFECT_CTRL_1                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_effectController1 ( channel, value );
		}
	}
	public static final int CC_EFFECT_CTRL_2                          = 13 ;
	public static final MetroMidiControlEffectController2 MIDI_EFFECT_CTRL_2  = new MetroMidiControlEffectController2();
	public static final class MetroMidiControlEffectController2 extends MetroMidiControlChangeBase {
		{
			this.shortName = "ec2";
			this.longName = "effect-controller-2";
			this.shortDescription = "Effect Controller 2";
			this.longDescription = "Usually used to control a parameter of an effect within the synth/workstation.";
			this.controlNumber = CC_EFFECT_CTRL_2                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_effectController2 ( channel, value );
		}
	}
	public static final int CC_SUSTAIN_PEDAL                          = 64 ;
	public static final MetroMidiControlSustainPedal MIDI_SUSTAIN_PEDAL  = new MetroMidiControlSustainPedal();
	public static final class MetroMidiControlSustainPedal extends MetroMidiControlChangeBase {
		{
			this.shortName = "sp";
			this.longName = "sustain-pedal";
			this.shortDescription = "Damper Pedal / Sustain Pedal";
			this.longDescription = "On/Off switch that controls sustain. (See also Sostenuto CC 66)0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_SUSTAIN_PEDAL                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_sustainPedal ( channel, value );
		}
	}
	public static final int CC_PORTAMENTO_SWITCH                      = 65 ;
	public static final MetroMidiControlPortamentoSwitch MIDI_PORTAMENTO_SWITCH  = new MetroMidiControlPortamentoSwitch();
	public static final class MetroMidiControlPortamentoSwitch extends MetroMidiControlChangeBase {
		{
			this.shortName = "ps";
			this.longName = "portamento-switch";
			this.shortDescription = "Portamento On/Off Switch";
			this.longDescription = "On/Off switch0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_PORTAMENTO_SWITCH                      ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_portamentoSwitch ( channel, value );
		}
	}
	public static final int CC_SOSTENUTO_SWITCH                       = 66 ;
	public static final MetroMidiControlSostenutoSwitch MIDI_SOSTENUTO_SWITCH  = new MetroMidiControlSostenutoSwitch();
	public static final class MetroMidiControlSostenutoSwitch extends MetroMidiControlChangeBase {
		{
			this.shortName = "sos-s";
			this.longName = "sostenuto-switch";
			this.shortDescription = "Sostenuto On/Off Switch";
			this.longDescription = "On/Off switch – Like the Sustain controller (CC 64), However it only holds notes that were “On” when the pedal was pressed. People use it to “hold” chords” and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_SOSTENUTO_SWITCH                       ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_sostenutoSwitch ( channel, value );
		}
	}
	public static final int CC_SOFT_PEDAL_SWITCH                      = 67 ;
	public static final MetroMidiControlPedalSwitch MIDI_SOFT_PEDAL_SWITCH  = new MetroMidiControlPedalSwitch();
	public static final class MetroMidiControlPedalSwitch extends MetroMidiControlChangeBase {
		{
			this.shortName = "soft-pedal";
			this.longName = "soft-pedal-switch";
			this.shortDescription = "Soft Pedal On/Off Switch";
			this.longDescription = "On/Off switch- Lowers the volume of notes played.0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_SOFT_PEDAL_SWITCH                      ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_pedalSwitch ( channel, value );
		}
	}
	public static final int CC_LEGATO_FOOTSWITCH                      = 68 ;
	public static final MetroMidiControlLegatoSwitch MIDI_LEGATO_FOOTSWITCH  = new MetroMidiControlLegatoSwitch();
	public static final class MetroMidiControlLegatoSwitch extends MetroMidiControlChangeBase {
		{
			this.shortName = "ls";
			this.longName = "legato-switch";
			this.shortDescription = "Legato FootSwitch";
			this.longDescription = "On/Off switch- Turns Legato effect between 2 subsequent notes On or Off.0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_LEGATO_FOOTSWITCH                      ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_legatoSwitch ( channel, value );
		}
	}
	public static final int CC_HOLD_2                                 = 69 ;
	public static final MetroMidiControlHold2 MIDI_HOLD_2  = new MetroMidiControlHold2();
	public static final class MetroMidiControlHold2 extends MetroMidiControlChangeBase {
		{
			this.shortName = "h2";
			this.longName = "hold-2";
			this.shortDescription = "Hold 2";
			this.longDescription = "Another way to “hold notes” (see MIDI CC 64 and MIDI CC 66). However notes fade out according to their release parameter rather than when the pedal is released.";
			this.controlNumber = CC_HOLD_2                                 ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_hold2 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_01                          = 70 ;
	public static final MetroMidiControlSoundController1 MIDI_SOUND_CTRL_01  = new MetroMidiControlSoundController1();
	public static final class MetroMidiControlSoundController1 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc1";
			this.longName = "sound-controller-1";
			this.shortDescription = "Sound Controller 1";
			this.longDescription = "Usually controls the way a sound is produced. Default = Sound Variation.";
			this.controlNumber = CC_SOUND_CTRL_01                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController1 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_02                          = 71 ;
	public static final MetroMidiControlSoundController2 MIDI_SOUND_CTRL_02  = new MetroMidiControlSoundController2();
	public static final class MetroMidiControlSoundController2 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc2";
			this.longName = "sound-controller-2";
			this.shortDescription = "Sound Controller 2";
			this.longDescription = "Allows shaping the Voltage Controlled Filter (VCF). Default = Resonance -also(Timbre or Harmonics)";
			this.controlNumber = CC_SOUND_CTRL_02                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController2 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_03                          = 72 ;
	public static final MetroMidiControlSoundController3 MIDI_SOUND_CTRL_03  = new MetroMidiControlSoundController3();
	public static final class MetroMidiControlSoundController3 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc3";
			this.longName = "sound-controller-3";
			this.shortDescription = "Sound Controller 3";
			this.longDescription = "Controls release time of the Voltage controlled Amplifier (VCA). Default = Release Time.";
			this.controlNumber = CC_SOUND_CTRL_03                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController3 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_04                          = 73 ;
	public static final MetroMidiControlSoundController4 MIDI_SOUND_CTRL_04  = new MetroMidiControlSoundController4();
	public static final class MetroMidiControlSoundController4 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc4";
			this.longName = "sound-controller-4";
			this.shortDescription = "Sound Controller 4";
			this.longDescription = "Controls the “Attack’ of a sound. The attack is the amount of time it takes forthe sound to reach maximum amplitude.";
			this.controlNumber = CC_SOUND_CTRL_04                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController4 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_05                          = 74 ;
	public static final MetroMidiControlSoundController5 MIDI_SOUND_CTRL_05  = new MetroMidiControlSoundController5();
	public static final class MetroMidiControlSoundController5 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc5";
			this.longName = "sound-controller-5";
			this.shortDescription = "Sound Controller 5";
			this.longDescription = "Controls VCFs cutoff frequency of the filter.";
			this.controlNumber = CC_SOUND_CTRL_05                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController5 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_06                          = 75 ;
	public static final MetroMidiControlSoundController6 MIDI_SOUND_CTRL_06  = new MetroMidiControlSoundController6();
	public static final class MetroMidiControlSoundController6 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc6";
			this.longName = "sound-controller-6";
			this.shortDescription = "Sound Controller 6";
			this.longDescription = "Generic – Some manufacturers may use to further shave their sounds.";
			this.controlNumber = CC_SOUND_CTRL_06                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController6 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_07                          = 76 ;
	public static final MetroMidiControlSoundController7 MIDI_SOUND_CTRL_07  = new MetroMidiControlSoundController7();
	public static final class MetroMidiControlSoundController7 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc7";
			this.longName = "sound-controller-7";
			this.shortDescription = "Sound Controller 7";
			this.longDescription = "Generic – Some manufacturers may use to further shave their sounds.";
			this.controlNumber = CC_SOUND_CTRL_07                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController7 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_08                          = 77 ;
	public static final MetroMidiControlSoundController8 MIDI_SOUND_CTRL_08  = new MetroMidiControlSoundController8();
	public static final class MetroMidiControlSoundController8 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc8";
			this.longName = "sound-controller-8";
			this.shortDescription = "Sound Controller 8";
			this.longDescription = "Generic – Some manufacturers may use to further shave their sounds.";
			this.controlNumber = CC_SOUND_CTRL_08                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController8 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_09                          = 78 ;
	public static final MetroMidiControlSoundController9 MIDI_SOUND_CTRL_09  = new MetroMidiControlSoundController9();
	public static final class MetroMidiControlSoundController9 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc9";
			this.longName = "sound-controller-9";
			this.shortDescription = "Sound Controller 9";
			this.longDescription = "Generic – Some manufacturers may use to further shave their sounds.";
			this.controlNumber = CC_SOUND_CTRL_09                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController9 ( channel, value );
		}
	}
	public static final int CC_SOUND_CTRL_10                          = 79 ;
	public static final MetroMidiControlSoundController10 MIDI_SOUND_CTRL_10  = new MetroMidiControlSoundController10();
	public static final class MetroMidiControlSoundController10 extends MetroMidiControlChangeBase {
		{
			this.shortName = "sc10";
			this.longName = "sound-controller-10";
			this.shortDescription = "Sound Controller 10";
			this.longDescription = "Generic – Some manufacturers may use to further shave their sounds.";
			this.controlNumber = CC_SOUND_CTRL_10                          ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_soundController10 ( channel, value );
		}
	}
	public static final int CC_GENERAL_PURPOSE_01                     = 80 ;
	public static final MetroMidiControlGeneralPurpose01 MIDI_GENERAL_PURPOSE_01  = new MetroMidiControlGeneralPurpose01();
	public static final class MetroMidiControlGeneralPurpose01 extends MetroMidiControlChangeBase {
		{
			this.shortName = "gp01";
			this.longName = "general-purpose-cc-01";
			this.shortDescription = "General Purpose MIDI CC Controller";
			this.longDescription = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_GENERAL_PURPOSE_01                     ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_generalPurpose01 ( channel, value );
		}
	}
	public static final int CC_GENERAL_PURPOSE_02                     = 81 ;
	public static final MetroMidiControlGeneralPurpose02 MIDI_GENERAL_PURPOSE_02  = new MetroMidiControlGeneralPurpose02();
	public static final class MetroMidiControlGeneralPurpose02 extends MetroMidiControlChangeBase {
		{
			this.shortName = "gp02";
			this.longName = "general-purpose-cc-02";
			this.shortDescription = "General Purpose MIDI CC Controller";
			this.longDescription = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_GENERAL_PURPOSE_02                     ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_generalPurpose02 ( channel, value );
		}
	}
	public static final int CC_GENERAL_PURPOSE_03                     = 82 ;
	public static final MetroMidiControlGeneralPurpose03 MIDI_GENERAL_PURPOSE_03  = new MetroMidiControlGeneralPurpose03();
	public static final class MetroMidiControlGeneralPurpose03 extends MetroMidiControlChangeBase {
		{
			this.shortName = "gp03";
			this.longName = "general-purpose-cc-03";
			this.shortDescription = "General PurposeMIDI CC Controller";
			this.longDescription = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_GENERAL_PURPOSE_03                     ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_generalPurpose03 ( channel, value );
		}
	}
	public static final int CC_GENERAL_PURPOSE_04                     = 83 ;
	public static final MetroMidiControlGeneralPurpose04 MIDI_GENERAL_PURPOSE_04  = new MetroMidiControlGeneralPurpose04();
	public static final class MetroMidiControlGeneralPurpose04 extends MetroMidiControlChangeBase {
		{
			this.shortName = "gp04";
			this.longName = "general-purpose-cc-04";
			this.shortDescription = "General Purpose MIDI CC Controller";
			this.longDescription = "GenericOn/Off switch0 to 63 = Off, 64 to 127 = On";
			this.controlNumber = CC_GENERAL_PURPOSE_04                     ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_generalPurpose04 ( channel, value );
		}
	}
	public static final int CC_PORTAMENTO_CC_CTRL                     = 84 ;
	public static final MetroMidiControlPortamento MIDI_PORTAMENTO_CC_CTRL  = new MetroMidiControlPortamento();
	public static final class MetroMidiControlPortamento extends MetroMidiControlChangeBase {
		{
			this.shortName = "po";
			this.longName = "portamento";
			this.shortDescription = "Portamento CC Control";
			this.longDescription = "Controls the amount of Portamento.";
			this.controlNumber = CC_PORTAMENTO_CC_CTRL                     ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_portamento ( channel, value );
		}
	}
	public static final int CC_EFFECT_1_DEPTH                         = 91 ;
	public static final MetroMidiControlEffect1 MIDI_EFFECT_1_DEPTH  = new MetroMidiControlEffect1();
	public static final class MetroMidiControlEffect1 extends MetroMidiControlChangeBase {
		{
			this.shortName = "e1";
			this.longName = "effect-1";
			this.shortDescription = "Effect 1 Depth";
			this.longDescription = "Usually controls reverb send amount";
			this.controlNumber = CC_EFFECT_1_DEPTH                         ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_effect1 ( channel, value );
		}
	}
	public static final int CC_EFFECT_2_DEPTH                         = 92 ;
	public static final MetroMidiControlEffect2 MIDI_EFFECT_2_DEPTH  = new MetroMidiControlEffect2();
	public static final class MetroMidiControlEffect2 extends MetroMidiControlChangeBase {
		{
			this.shortName = "e2";
			this.longName = "effect-2";
			this.shortDescription = "Effect 2 Depth";
			this.longDescription = "Usually controls tremolo amount";
			this.controlNumber = CC_EFFECT_2_DEPTH                         ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_effect2 ( channel, value );
		}
	}
	public static final int CC_EFFECT_3_DEPTH                         = 93 ;
	public static final MetroMidiControlEffect3 MIDI_EFFECT_3_DEPTH  = new MetroMidiControlEffect3();
	public static final class MetroMidiControlEffect3 extends MetroMidiControlChangeBase {
		{
			this.shortName = "e3";
			this.longName = "effect-3";
			this.shortDescription = "Effect 3 Depth";
			this.longDescription = "Usually controls chorus amount";
			this.controlNumber = CC_EFFECT_3_DEPTH                         ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_effect3 ( channel, value );
		}
	}
	public static final int CC_EFFECT_4_DEPTH                         = 94 ;
	public static final MetroMidiControlEffect4 MIDI_EFFECT_4_DEPTH  = new MetroMidiControlEffect4();
	public static final class MetroMidiControlEffect4 extends MetroMidiControlChangeBase {
		{
			this.shortName = "e4";
			this.longName = "effect-4";
			this.shortDescription = "Effect 4 Depth";
			this.longDescription = "Usually controls detune amount";
			this.controlNumber = CC_EFFECT_4_DEPTH                         ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_effect4 ( channel, value );
		}
	}
	public static final int CC_EFFECT_5_DEPTH                         = 95 ;
	public static final MetroMidiControlEffect5 MIDI_EFFECT_5_DEPTH  = new MetroMidiControlEffect5();
	public static final class MetroMidiControlEffect5 extends MetroMidiControlChangeBase {
		{
			this.shortName = "e5";
			this.longName = "effect-5";
			this.shortDescription = "Effect 5 Depth";
			this.longDescription = "Usually controls phaser amount";
			this.controlNumber = CC_EFFECT_5_DEPTH                         ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_effect5 ( channel, value );
		}
	}
	public static final int CC_DATA_INCREMENT                         = 96 ;
	public static final MetroMidiControlDataIncrement MIDI_DATA_INCREMENT  = new MetroMidiControlDataIncrement();
	public static final class MetroMidiControlDataIncrement extends MetroMidiControlChangeBase {
		{
			this.shortName = "inc";
			this.longName = "data-increment";
			this.shortDescription = "(+1) Data Increment";
			this.longDescription = "Usually used to increment data for RPN and NRPN messages.";
			this.controlNumber = CC_DATA_INCREMENT;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_dataIncrement ( channel, value );
		}
	}
	public static final int CC_DATA_DECREMENT                         = 97 ;
	public static final MetroMidiControlDataDecrement MIDI_DATA_DECREMENT  = new MetroMidiControlDataDecrement();
	public static final class MetroMidiControlDataDecrement extends MetroMidiControlChangeBase {
		{
			this.shortName = "dec";
			this.longName = "data-decrement";
			this.shortDescription = "(-1) Data Decrement";
			this.longDescription = "Usually used to decrement data for RPN and NRPN messages.";
			this.controlNumber = CC_DATA_DECREMENT ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_dataDecrement ( channel, value );
		}
	}
	public static final int CC_NRPN_LSB                               = 98 ;
	public static final MetroMidiControlNrpnLsb MIDI_NRPN_LSB  = new MetroMidiControlNrpnLsb();
	public static final class MetroMidiControlNrpnLsb extends MetroMidiControlChangeBase {
		{
			this.shortName = "nrpn-l";
			this.longName = "nrpn-lsb";
			this.shortDescription = "Non-Registered Parameter Number LSB (NRPN)";
			this.longDescription = "For controllers 6, 38, 96, and 97, it selects the NRPN parameter.";
			this.controlNumber = CC_NRPN_LSB                               ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_nrpnLsb ( channel, value );
		}
	}
	public static final int CC_NRPN_MSB                               = 99 ;
	public static final MetroMidiControlNrpnMsb MIDI_NRPN_MSB  = new MetroMidiControlNrpnMsb();
	public static final class MetroMidiControlNrpnMsb extends MetroMidiControlChangeBase {
		{
			this.shortName = "nrpn-m";
			this.longName = "nrpn-msb";
			this.shortDescription = "Non-Registered Parameter Number MSB (NRPN)";
			this.longDescription = "For controllers 6, 38, 96, and 97, it selects the NRPN parameter.";
			this.controlNumber = CC_NRPN_MSB                               ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_nrpnMsb ( channel, value );
		}
	}
	public static final int CC_RPN_LSB                                = 100;
	public static final MetroMidiControlRpnLsb MIDI_RPN_LSB  = new MetroMidiControlRpnLsb();
	public static final class MetroMidiControlRpnLsb extends MetroMidiControlChangeBase {
		{
			this.shortName = "rpn-l";
			this.longName = "rpn-lsb";
			this.shortDescription = "Registered Parameter Number LSB (RPN)";
			this.longDescription = "For controllers 6, 38, 96, and 97, it selects the RPN parameter.";
			this.controlNumber = CC_RPN_LSB                                ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_rpnLsb ( channel, value );
		}
	}
	public static final int CC_RPN_MSB                                = 101;
	public static final MetroMidiControlRpnMsb MIDI_RPN_MSB  = new MetroMidiControlRpnMsb();
	public static final class MetroMidiControlRpnMsb extends MetroMidiControlChangeBase {
		{
			this.shortName = "rpn-m";
			this.longName = "rpn-msb";
			this.shortDescription = "Registered Parameter Number MSB (RPN)";
			this.longDescription = "For controllers 6, 38, 96, and 97, it selects the RPN parameter.";
			this.controlNumber = CC_RPN_MSB                                ;
		}
		public <T> T execute( MetroMidiReceiver<T> receiver, int channel, int value ) {
			return receiver.cc_rpnMsb ( channel, value );
		}
	}
	
	static {
		putInfo( new MetroMidiNoteOn());
		putInfo( new MetroMidiNoteOff());

		////////////////////////////////////////////////////////////////////////////////////////////////////////////

		putInfo( new MetroMidiKeyPressure());

		putInfo( new MetroMidiControlChange());
		putInfo( new MetroMidiProgramChange());
		putInfo( new MetroMidiChannelPressure());
		putInfo( new MetroMidiPitchBend());

		putInfo( new MetroMidiAllSoundOff());
		putInfo( new MetroMidiResetAllControllers());
		putInfo( new MetroMidiLocalControls());
		putInfo( new MetroMidiAllNoteOff());
		putInfo( new MetroMidiOmniModeOff());
		putInfo( new MetroMidiOmniModeOn());
		putInfo( new MetroMidiMonoModeOn());
		putInfo( new MetroMidiPolyModeOn());
		putInfo( new MetroMidiSongPositionPointer());
		putInfo( new MetroMidiSongSelect());
		putInfo( new MetroMidiEndOfExclusive());
		putInfo( new MetroMidiClock());
		putInfo( new MetroMidiStart());
		putInfo( new MetroMidiContinue());
		putInfo( new MetroMidiStop());
		putInfo( new MetroMidiReset());


		/*
		 * Control Changes
		 */

		//                                   
		putInfo( new MetroMidiControlBankSelect());
		
		//                                    
		putInfo( new MetroMidiControlModulation());
		
		//                             
		putInfo( new MetroMidiControlBreathController());
		
		//                               
		putInfo( new MetroMidiControlFootController());
		
		//                                
		putInfo( new MetroMidiControlPortamentoTime());
		
		//          
		putInfo( new MetroMidiControlDataEntryMsb());
		
		//                                        
		putInfo( new MetroMidiControlVolume());
		
		//                                       
		putInfo( new MetroMidiControlBalance());
		
		//                                           
		putInfo( new MetroMidiControlPan());
		
		//                                    
		putInfo( new MetroMidiControlExpression());
		
		//                           
		putInfo( new MetroMidiControlEffectController1());
		
		//                           
		putInfo( new MetroMidiControlEffectController2());
		
		//                   
		putInfo( new MetroMidiControlSustainPedal());
		
		//                      
		putInfo( new MetroMidiControlPortamentoSwitch());
		
		//                       
		putInfo( new MetroMidiControlSostenutoSwitch());
		
		//                      
		putInfo( new MetroMidiControlPedalSwitch());
		
		//                             
		putInfo( new MetroMidiControlLegatoSwitch());
		
		//                                        
		putInfo( new MetroMidiControlHold2());
		
		//                            
		putInfo( new MetroMidiControlSoundController1());
		
		//                            
		putInfo( new MetroMidiControlSoundController2());
		
		//                            
		putInfo( new MetroMidiControlSoundController3());
		
		//                            
		putInfo( new MetroMidiControlSoundController4());
		
		//                            
		putInfo( new MetroMidiControlSoundController5());
		
		//                            
		putInfo( new MetroMidiControlSoundController6());
		
		//                            
		putInfo( new MetroMidiControlSoundController7());
		
		//                            
		putInfo( new MetroMidiControlSoundController8());
		
		//                            
		putInfo( new MetroMidiControlSoundController9());
		
		//                           
		putInfo( new MetroMidiControlSoundController10());
		
		//             
		putInfo( new MetroMidiControlGeneralPurpose01());
		
		//            
		putInfo( new MetroMidiControlGeneralPurpose02());
		
		//             
		putInfo( new MetroMidiControlGeneralPurpose03());
		
		//            
		putInfo( new MetroMidiControlGeneralPurpose04());
		
		//                         
		putInfo( new MetroMidiControlPortamento());
		
		//                                
		putInfo( new MetroMidiControlEffect1());
		
		//                                
		putInfo( new MetroMidiControlEffect2());
		
		//                                
		putInfo( new MetroMidiControlEffect3());
		
		//                                
		putInfo( new MetroMidiControlEffect4());
		
		//                                
		putInfo( new MetroMidiControlEffect5());
		
		//                           
		putInfo( new MetroMidiControlDataIncrement());
		
		//                           
		putInfo( new MetroMidiControlDataDecrement());
		
		//    
		putInfo( new MetroMidiControlNrpnLsb());
		
		//    
		putInfo( new MetroMidiControlNrpnMsb());
		
		//         
		putInfo( new MetroMidiControlRpnLsb());
		
		//         
		putInfo( new MetroMidiControlRpnMsb());
		
	}
}

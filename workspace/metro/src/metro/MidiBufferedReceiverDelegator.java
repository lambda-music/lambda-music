package metro;

public class MidiBufferedReceiverDelegator<T> implements MetroMidiReceiver<T> {
    private final MetroMidiBufferedReceiver receiver;
    public MidiBufferedReceiverDelegator( MetroMidiBufferedReceiver receiver ) {
        this.receiver = receiver;
    }
    
    // basic 
    public void noteOn( double offset, MetroPort port, int channel, int note, double velocity) {
        receiver.noteOn( offset, port, channel, note, velocity);
    }
    public void noteOn( double offset, MetroPort port, int channel, int note, int velocity) {
        receiver.noteOn( offset, port, channel, note, velocity);
    }
    public void noteOff( double offset, MetroPort port, int channel, int note, double velocity) {
        receiver.noteOff( offset, port, channel, note, velocity);
    }
    public void noteOff( double offset, MetroPort port, int channel, int note, int velocity) {
        receiver.noteOff( offset, port, channel, note, velocity);
    }
    public void keyPressure( double offset, MetroPort port, int channel, int note, double value) {
        receiver.keyPressure( offset, port, channel, note, value);
    }
    public void keyPressure( double offset, MetroPort port, int channel, int note, int value) {
        receiver.keyPressure( offset, port, channel, note, value);
    }
    public void controlChange( double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
        receiver.controlChange( offset, port, channel, controlNumber, controlValue);
    }
    public void programChange( double offset, MetroPort port, int channel, int value) {
        receiver.programChange( offset, port, channel, value);
    }
    public void channelPressure( double offset, MetroPort port, int channel, double value) {
        receiver.channelPressure( offset, port, channel, value);
    }
    public void channelPressure( double offset, MetroPort port, int channel, int value) {
        receiver.channelPressure( offset, port, channel, value);
    }
    public void pitchBend( double offset, MetroPort port, int channel, double value) {
        receiver.pitchBend( offset, port, channel, value);
    }
    public void pitchBend( double offset, MetroPort port, int channel, int value) {
        receiver.pitchBend( offset, port, channel, value);
    }
    public void cc_allSoundOff( double offset, MetroPort port, int channel) {
        receiver.cc_allSoundOff( offset, port, channel);
    }
    public void cc_resetAllControllers( double offset, MetroPort port, int channel) {
        receiver.cc_resetAllControllers( offset, port, channel);
    }
    public void cc_localControls( double offset, MetroPort port, int channel, boolean value) {
        receiver.cc_localControls( offset, port, channel, value);
    }
    public void cc_allNoteOff( double offset, MetroPort port, int channel) {
        receiver.cc_allNoteOff( offset, port, channel);
    }
    public void cc_omniModeOff( double offset, MetroPort port, int channel) {
        receiver.cc_omniModeOff( offset, port, channel);
    }
    public void cc_omniModeOn( double offset, MetroPort port, int channel) {
        receiver.cc_omniModeOn( offset, port, channel);
    }
    public void cc_monoModeOn( double offset, MetroPort port, int channel) {
        receiver.cc_monoModeOn( offset, port, channel);
    }
    public void cc_polyModeOn( double offset, MetroPort port, int channel) {
        receiver.cc_polyModeOn( offset, port, channel);
    }
    public void songPositionPointer( double offset, MetroPort port, int value) {
        receiver.songPositionPointer( offset, port, value);
    }
    public void songSelect( double offset, MetroPort port, int value) {
        receiver.songSelect( offset, port, value);
    }
    public void endOfExclusive(  double offset, MetroPort port ) {
        receiver.endOfExclusive( offset, port );
    }
    public void clock(  double offset, MetroPort port ) {
        receiver.clock( offset, port );
    }
    public void start(  double offset, MetroPort port ){
        receiver.start( offset, port );
    }
    public void cont(  double offset, MetroPort port ) {
        receiver.cont( offset, port );
    }
    public void stop(  double offset, MetroPort port ) {
        receiver.stop( offset, port );
    }
    public void reset(  double offset, MetroPort port ) {
        receiver.reset( offset, port );
    }
    public void cc_bankSelect( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_bankSelect( offset, port, channel, controlValue);
    }
    public void cc_modulation( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_modulation( offset, port, channel, controlValue);
    }
    public void cc_breathController( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_breathController( offset, port, channel, controlValue);
    }
    public void cc_footController( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_footController( offset, port, channel, controlValue);
    }
    public void cc_portamentoTime( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_portamentoTime( offset, port, channel, controlValue);
    }
    public void cc_dataEntryMsb( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_dataEntryMsb( offset, port, channel, controlValue);
    }
    public void cc_volume( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_volume( offset, port, channel, controlValue);
    }
    public void cc_balance( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_balance( offset, port, channel, controlValue);
    }
    public void cc_pan( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_pan( offset, port, channel, controlValue);
    }
    public void cc_expression( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_expression( offset, port, channel, controlValue);
    }
    public void cc_effectController1( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_effectController1( offset, port, channel, controlValue);
    }
    public void cc_effectController2( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_effectController2( offset, port, channel, controlValue);
    }
    public void cc_sustainPedal( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_sustainPedal( offset, port, channel, controlValue);
    }
    public void cc_portamentoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_portamentoSwitch( offset, port, channel, controlValue);
    }
    public void cc_sostenutoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_sostenutoSwitch( offset, port, channel, controlValue);
    }
    public void cc_pedalSwitch( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_pedalSwitch( offset, port, channel, controlValue);
    }
    public void cc_legatoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_legatoSwitch( offset, port, channel, controlValue);
    }
    public void cc_hold2( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_hold2( offset, port, channel, controlValue);
    }
    public void cc_soundController1( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController1( offset, port, channel, controlValue);
    }
    public void cc_soundController2( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController2( offset, port, channel, controlValue);
    }
    public void cc_soundController3( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController3( offset, port, channel, controlValue);
    }
    public void cc_soundController4( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController4( offset, port, channel, controlValue);
    }
    public void cc_soundController5( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController5( offset, port, channel, controlValue);
    }
    public void cc_soundController6( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController6( offset, port, channel, controlValue);
    }
    public void cc_soundController7( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController7( offset, port, channel, controlValue);
    }
    public void cc_soundController8( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController8( offset, port, channel, controlValue);
    }
    public void cc_soundController9( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController9( offset, port, channel, controlValue);
    }
    public void cc_soundController10( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_soundController10( offset, port, channel, controlValue);
    }
    public void cc_generalPurpose01( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_generalPurpose01( offset, port, channel, controlValue);
    }
    public void cc_generalPurpose02( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_generalPurpose02( offset, port, channel, controlValue);
    }
    public void cc_generalPurpose03( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_generalPurpose03( offset, port, channel, controlValue);
    }
    public void cc_generalPurpose04( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_generalPurpose04( offset, port, channel, controlValue);
    }
    public void cc_portamento( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_portamento( offset, port, channel, controlValue);
    }
    public void cc_effect1( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_effect1( offset, port, channel, controlValue);
    }
    public void cc_effect2( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_effect2( offset, port, channel, controlValue);
    }
    public void cc_effect3( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_effect3( offset, port, channel, controlValue);
    }
    public void cc_effect4( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_effect4( offset, port, channel, controlValue);
    }
    public void cc_effect5( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_effect5( offset, port, channel, controlValue);
    }
    public void cc_dataIncrement( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_dataIncrement( offset, port, channel, controlValue);
    }
    public void cc_dataDecrement( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_dataDecrement( offset, port, channel, controlValue);
    }
    public void cc_nrpnLsb( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_nrpnLsb( offset, port, channel, controlValue);
    }
    public void cc_nrpnMsb( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_nrpnMsb( offset, port, channel, controlValue);
    }
    public void cc_rpnLsb( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_rpnLsb( offset, port, channel, controlValue);
    }
    public void cc_rpnMsb( double offset, MetroPort port, int channel, int controlValue) {
        receiver.cc_rpnMsb( offset, port, channel, controlValue);
    }
}

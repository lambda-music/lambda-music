package metro;

public abstract  class MidiReceiverDelegator<T> implements MetroMidiBufferedReceiver {
    private final MetroMidiReceiver<T> receiver;
    public MidiReceiverDelegator( MetroMidiReceiver<T> receiver ) {
        this.receiver = receiver;
    }
    public abstract void midiEvent( String id, double offset, MetroPort outputPort, T data );
    
    // basic 
    public void noteOn( double offset, MetroPort port, int channel, int note, double velocity) {
        this.midiEvent( "noteOn", offset, port, receiver.noteOn(channel, note, velocity));
    }
    public void noteOn( double offset, MetroPort port, int channel, int note, int velocity) {
        this.midiEvent( "noteOn", offset, port, receiver.noteOn(channel, note, velocity));
    }
    public void noteOff( double offset, MetroPort port, int channel, int note, double velocity) {
        this.midiEvent( "noteOff", offset, port, receiver.noteOff(channel, note, velocity));
    }
    public void noteOff( double offset, MetroPort port, int channel, int note, int velocity) {
        this.midiEvent( "noteOff", offset, port, receiver.noteOff(channel, note, velocity));
    }
    public void keyPressure( double offset, MetroPort port, int channel, int note, double value) {
        this.midiEvent( "keyPressure", offset, port, receiver.keyPressure(channel, note, value));
    }
    public void keyPressure( double offset, MetroPort port, int channel, int note, int value) {
        this.midiEvent( "keyPressure", offset, port, receiver.keyPressure(channel, note, value));
    }
    public void controlChange( double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
        this.midiEvent( "controlChange", offset, port, receiver.controlChange(channel, controlNumber, controlValue));
    }
    public void programChange( double offset, MetroPort port, int channel, int value) {
        this.midiEvent( "programChange", offset, port, receiver.programChange(channel, value));
    }
    public void channelPressure( double offset, MetroPort port, int channel, double value) {
        this.midiEvent( "channelPressure", offset, port, receiver.channelPressure(channel, value));
    }
    public void channelPressure( double offset, MetroPort port, int channel, int value) {
        this.midiEvent( "channelPressure", offset, port, receiver.channelPressure(channel, value));
    }
    public void pitchBend( double offset, MetroPort port, int channel, double value) {
        this.midiEvent( "pitchBend", offset, port, receiver.pitchBend(channel, value));
    }
    public void pitchBend( double offset, MetroPort port, int channel, int value) {
        this.midiEvent( "pitchBend", offset, port, receiver.pitchBend(channel, value));
    }
    public void cc_allSoundOff( double offset, MetroPort port, int channel) {
        this.midiEvent( "cc_allSoundOff", offset, port, receiver.cc_allSoundOff(channel));
    }
    public void cc_resetAllControllers( double offset, MetroPort port, int channel) {
        this.midiEvent( "cc_resetAllControllers", offset, port, receiver.cc_resetAllControllers(channel));
    }
    public void cc_localControls( double offset, MetroPort port, int channel, boolean value) {
        this.midiEvent( "cc_localControls", offset, port, receiver.cc_localControls(channel, value));
    }
    public void cc_allNoteOff( double offset, MetroPort port, int channel) {
        this.midiEvent( "cc_allNoteOff", offset, port, receiver.cc_allNoteOff(channel));
    }
    public void cc_omniModeOff( double offset, MetroPort port, int channel) {
        this.midiEvent( "cc_omniModeOff", offset, port, receiver.cc_omniModeOff(channel));
    }
    public void cc_omniModeOn( double offset, MetroPort port, int channel) {
        this.midiEvent( "cc_omniModeOn", offset, port, receiver.cc_omniModeOn(channel));
    }
    public void cc_monoModeOn( double offset, MetroPort port, int channel) {
        this.midiEvent( "cc_monoModeOn", offset, port, receiver.cc_monoModeOn(channel));
    }
    public void cc_polyModeOn( double offset, MetroPort port, int channel) {
        this.midiEvent( "cc_polyModeOn", offset, port, receiver.cc_polyModeOn(channel));
    }
    public void songPositionPointer( double offset, MetroPort port, int value) {
        this.midiEvent( "songPositionPointer", offset, port, receiver.songPositionPointer(value));
    }
    public void songSelect( double offset, MetroPort port, int value) {
        this.midiEvent( "songSelect", offset, port, receiver.songSelect(value));
    }
    public void endOfExclusive(  double offset, MetroPort port ) {
        this.midiEvent( "endOfExclusive", offset, port, receiver.endOfExclusive());
    }
    public void clock(  double offset, MetroPort port ) {
        this.midiEvent( "clock", offset, port, receiver.clock());
    }
    public void start(  double offset, MetroPort port ){
        this.midiEvent( "start", offset, port, receiver.start());
    }
    public void cont(  double offset, MetroPort port ) {
        this.midiEvent( "cont", offset, port, receiver.cont());
    }
    public void stop(  double offset, MetroPort port ) {
        this.midiEvent( "stop", offset, port, receiver.stop());
    }
    public void reset(  double offset, MetroPort port ) {
        this.midiEvent( "reset", offset, port, receiver.reset());
    }
    public void cc_bankSelect( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_bankSelect", offset, port, receiver.cc_bankSelect(channel, controlValue));
    }
    public void cc_modulation( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_modulation", offset, port, receiver.cc_modulation(channel, controlValue));
    }
    public void cc_breathController( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_breathController", offset, port, receiver.cc_breathController(channel, controlValue));
    }
    public void cc_footController( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_footController", offset, port, receiver.cc_footController(channel, controlValue));
    }
    public void cc_portamentoTime( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_portamentoTime", offset, port, receiver.cc_portamentoTime(channel, controlValue));
    }
    public void cc_dataEntryMsb( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_dataEntryMsb", offset, port, receiver.cc_dataEntryMsb(channel, controlValue));
    }
    public void cc_volume( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_volume", offset, port, receiver.cc_volume(channel, controlValue));
    }
    public void cc_balance( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_balance", offset, port, receiver.cc_balance(channel, controlValue));
    }
    public void cc_pan( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_pan", offset, port, receiver.cc_pan(channel, controlValue));
    }
    public void cc_expression( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_expression", offset, port, receiver.cc_expression(channel, controlValue));
    }
    public void cc_effectController1( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_effectController1", offset, port, receiver.cc_effectController1(channel, controlValue));
    }
    public void cc_effectController2( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_effectController2", offset, port, receiver.cc_effectController2(channel, controlValue));
    }
    public void cc_sustainPedal( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_sustainPedal", offset, port, receiver.cc_sustainPedal(channel, controlValue));
    }
    public void cc_portamentoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_portamentoSwitch", offset, port, receiver.cc_portamentoSwitch(channel, controlValue));
    }
    public void cc_sostenutoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_sostenutoSwitch", offset, port, receiver.cc_sostenutoSwitch(channel, controlValue));
    }
    public void cc_pedalSwitch( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_pedalSwitch", offset, port, receiver.cc_pedalSwitch(channel, controlValue));
    }
    public void cc_legatoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_legatoSwitch", offset, port, receiver.cc_legatoSwitch(channel, controlValue));
    }
    public void cc_hold2( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_hold2", offset, port, receiver.cc_hold2(channel, controlValue));
    }
    public void cc_soundController1( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController1", offset, port, receiver.cc_soundController1(channel, controlValue));
    }
    public void cc_soundController2( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController2", offset, port, receiver.cc_soundController2(channel, controlValue));
    }
    public void cc_soundController3( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController3", offset, port, receiver.cc_soundController3(channel, controlValue));
    }
    public void cc_soundController4( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController4", offset, port, receiver.cc_soundController4(channel, controlValue));
    }
    public void cc_soundController5( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController5", offset, port, receiver.cc_soundController5(channel, controlValue));
    }
    public void cc_soundController6( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController6", offset, port, receiver.cc_soundController6(channel, controlValue));
    }
    public void cc_soundController7( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController7", offset, port, receiver.cc_soundController7(channel, controlValue));
    }
    public void cc_soundController8( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController8", offset, port, receiver.cc_soundController8(channel, controlValue));
    }
    public void cc_soundController9( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController9", offset, port, receiver.cc_soundController9(channel, controlValue));
    }
    public void cc_soundController10( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_soundController10", offset, port, receiver.cc_soundController10(channel, controlValue));
    }
    public void cc_generalPurpose01( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_generalPurpose01", offset, port, receiver.cc_generalPurpose01(channel, controlValue));
    }
    public void cc_generalPurpose02( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_generalPurpose02", offset, port, receiver.cc_generalPurpose02(channel, controlValue));
    }
    public void cc_generalPurpose03( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_generalPurpose03", offset, port, receiver.cc_generalPurpose03(channel, controlValue));
    }
    public void cc_generalPurpose04( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_generalPurpose04", offset, port, receiver.cc_generalPurpose04(channel, controlValue));
    }
    public void cc_portamento( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_portamento", offset, port, receiver.cc_portamento(channel, controlValue));
    }
    public void cc_effect1( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_effect1", offset, port, receiver.cc_effect1(channel, controlValue));
    }
    public void cc_effect2( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_effect2", offset, port, receiver.cc_effect2(channel, controlValue));
    }
    public void cc_effect3( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_effect3", offset, port, receiver.cc_effect3(channel, controlValue));
    }
    public void cc_effect4( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_effect4", offset, port, receiver.cc_effect4(channel, controlValue));
    }
    public void cc_effect5( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_effect5", offset, port, receiver.cc_effect5(channel, controlValue));
    }
    public void cc_dataIncrement( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_dataIncrement", offset, port, receiver.cc_dataIncrement(channel, controlValue));
    }
    public void cc_dataDecrement( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_dataDecrement", offset, port, receiver.cc_dataDecrement(channel, controlValue));
    }
    public void cc_nrpnLsb( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_nrpnLsb", offset, port, receiver.cc_nrpnLsb(channel, controlValue));
    }
    public void cc_nrpnMsb( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_nrpnMsb", offset, port, receiver.cc_nrpnMsb(channel, controlValue));
    }
    public void cc_rpnLsb( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_rpnLsb", offset, port, receiver.cc_rpnLsb(channel, controlValue));
    }
    public void cc_rpnMsb( double offset, MetroPort port, int channel, int controlValue) {
        this.midiEvent( "cc_rpnMsb", offset, port, receiver.cc_rpnMsb(channel, controlValue));
    }
    public void error( double offset, MetroPort port, String string) {
        this.midiEvent( "error", offset, port, receiver.error(string));
    }
}

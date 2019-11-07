package metro;

public abstract  class MetroBufferedToNonBufferedMidiReceiver<OUTER,INNER> implements MetroBufferedMidiReceiver<OUTER> {
    private final MetroMidiReceiver<INNER> receiver;
    public MetroBufferedToNonBufferedMidiReceiver( MetroMidiReceiver<INNER> receiver ) {
        this.receiver = receiver;
    }
    public abstract OUTER convertResult( String id, double offset, MetroPort outputPort, INNER data );

    // system
    private boolean endCalled=false;
    @Override
    public boolean endCalled() {
        return endCalled;
    }
    @Override
    public OUTER end() {
        this.endCalled = true;
        return this.convertResult( "end", 0, null, this.receiver.end() );
    }
    public OUTER error( double offset, MetroPort port, String string) {
        return this.convertResult( "error", offset, port, receiver.error(string));
    }

    // basic 
    public OUTER noteOn( double offset, MetroPort port, int channel, int note, double velocity) {
        return this.convertResult( "noteOn", offset, port, receiver.noteOn(channel, note, velocity));
    }
    public OUTER noteOn( double offset, MetroPort port, int channel, int note, int velocity) {
        return this.convertResult( "noteOn", offset, port, receiver.noteOn(channel, note, velocity));
    }
    public OUTER noteOff( double offset, MetroPort port, int channel, int note, double velocity) {
        return this.convertResult( "noteOff", offset, port, receiver.noteOff(channel, note, velocity));
    }
    public OUTER noteOff( double offset, MetroPort port, int channel, int note, int velocity) {
        return this.convertResult( "noteOff", offset, port, receiver.noteOff(channel, note, velocity));
    }
    public OUTER keyPressure( double offset, MetroPort port, int channel, int note, double value) {
        return this.convertResult( "keyPressure", offset, port, receiver.keyPressure(channel, note, value));
    }
    public OUTER keyPressure( double offset, MetroPort port, int channel, int note, int value) {
        return this.convertResult( "keyPressure", offset, port, receiver.keyPressure(channel, note, value));
    }
    public OUTER controlChange( double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
        return this.convertResult( "controlChange", offset, port, receiver.controlChange(channel, controlNumber, controlValue));
    }
    public OUTER programChange( double offset, MetroPort port, int channel, int value) {
        return this.convertResult( "programChange", offset, port, receiver.programChange(channel, value));
    }
    public OUTER channelPressure( double offset, MetroPort port, int channel, double value) {
        return this.convertResult( "channelPressure", offset, port, receiver.channelPressure(channel, value));
    }
    public OUTER channelPressure( double offset, MetroPort port, int channel, int value) {
        return this.convertResult( "channelPressure", offset, port, receiver.channelPressure(channel, value));
    }
    public OUTER pitchBend( double offset, MetroPort port, int channel, double value) {
        return this.convertResult( "pitchBend", offset, port, receiver.pitchBend(channel, value));
    }
    public OUTER pitchBend( double offset, MetroPort port, int channel, int value) {
        return this.convertResult( "pitchBend", offset, port, receiver.pitchBend(channel, value));
    }
    public OUTER cc_allSoundOff( double offset, MetroPort port, int channel) {
        return this.convertResult( "cc_allSoundOff", offset, port, receiver.cc_allSoundOff(channel));
    }
    public OUTER cc_resetAllControllers( double offset, MetroPort port, int channel) {
        return this.convertResult( "cc_resetAllControllers", offset, port, receiver.cc_resetAllControllers(channel));
    }
    public OUTER cc_localControls( double offset, MetroPort port, int channel, boolean value) {
        return this.convertResult( "cc_localControls", offset, port, receiver.cc_localControls(channel, value));
    }
    public OUTER cc_allNoteOff( double offset, MetroPort port, int channel) {
        return this.convertResult( "cc_allNoteOff", offset, port, receiver.cc_allNoteOff(channel));
    }
    public OUTER cc_omniModeOff( double offset, MetroPort port, int channel) {
        return this.convertResult( "cc_omniModeOff", offset, port, receiver.cc_omniModeOff(channel));
    }
    public OUTER cc_omniModeOn( double offset, MetroPort port, int channel) {
        return this.convertResult( "cc_omniModeOn", offset, port, receiver.cc_omniModeOn(channel));
    }
    public OUTER cc_monoModeOn( double offset, MetroPort port, int channel) {
        return this.convertResult( "cc_monoModeOn", offset, port, receiver.cc_monoModeOn(channel));
    }
    public OUTER cc_polyModeOn( double offset, MetroPort port, int channel) {
        return this.convertResult( "cc_polyModeOn", offset, port, receiver.cc_polyModeOn(channel));
    }
    public OUTER songPositionPointer( double offset, MetroPort port, int value) {
        return this.convertResult( "songPositionPointer", offset, port, receiver.songPositionPointer(value));
    }
    public OUTER songSelect( double offset, MetroPort port, int value) {
        return this.convertResult( "songSelect", offset, port, receiver.songSelect(value));
    }
    public OUTER endOfExclusive(  double offset, MetroPort port ) {
        return this.convertResult( "endOfExclusive", offset, port, receiver.endOfExclusive());
    }
    public OUTER clock(  double offset, MetroPort port ) {
        return this.convertResult( "clock", offset, port, receiver.clock());
    }
    public OUTER start(  double offset, MetroPort port ){
        return this.convertResult( "start", offset, port, receiver.start());
    }
    public OUTER cont(  double offset, MetroPort port ) {
        return this.convertResult( "cont", offset, port, receiver.cont());
    }
    public OUTER stop(  double offset, MetroPort port ) {
        return this.convertResult( "stop", offset, port, receiver.stop());
    }
    public OUTER reset(  double offset, MetroPort port ) {
        return this.convertResult( "reset", offset, port, receiver.reset());
    }
    public OUTER cc_bankSelect( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_bankSelect", offset, port, receiver.cc_bankSelect(channel, controlValue));
    }
    public OUTER cc_modulation( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_modulation", offset, port, receiver.cc_modulation(channel, controlValue));
    }
    public OUTER cc_breathController( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_breathController", offset, port, receiver.cc_breathController(channel, controlValue));
    }
    public OUTER cc_footController( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_footController", offset, port, receiver.cc_footController(channel, controlValue));
    }
    public OUTER cc_portamentoTime( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_portamentoTime", offset, port, receiver.cc_portamentoTime(channel, controlValue));
    }
    public OUTER cc_dataEntryMsb( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_dataEntryMsb", offset, port, receiver.cc_dataEntryMsb(channel, controlValue));
    }
    public OUTER cc_volume( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_volume", offset, port, receiver.cc_volume(channel, controlValue));
    }
    public OUTER cc_balance( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_balance", offset, port, receiver.cc_balance(channel, controlValue));
    }
    public OUTER cc_pan( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_pan", offset, port, receiver.cc_pan(channel, controlValue));
    }
    public OUTER cc_expression( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_expression", offset, port, receiver.cc_expression(channel, controlValue));
    }
    public OUTER cc_effectController1( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_effectController1", offset, port, receiver.cc_effectController1(channel, controlValue));
    }
    public OUTER cc_effectController2( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_effectController2", offset, port, receiver.cc_effectController2(channel, controlValue));
    }
    public OUTER cc_sustainPedal( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_sustainPedal", offset, port, receiver.cc_sustainPedal(channel, controlValue));
    }
    public OUTER cc_portamentoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_portamentoSwitch", offset, port, receiver.cc_portamentoSwitch(channel, controlValue));
    }
    public OUTER cc_sostenutoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_sostenutoSwitch", offset, port, receiver.cc_sostenutoSwitch(channel, controlValue));
    }
    public OUTER cc_pedalSwitch( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_pedalSwitch", offset, port, receiver.cc_pedalSwitch(channel, controlValue));
    }
    public OUTER cc_legatoSwitch( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_legatoSwitch", offset, port, receiver.cc_legatoSwitch(channel, controlValue));
    }
    public OUTER cc_hold2( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_hold2", offset, port, receiver.cc_hold2(channel, controlValue));
    }
    public OUTER cc_soundController1( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController1", offset, port, receiver.cc_soundController1(channel, controlValue));
    }
    public OUTER cc_soundController2( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController2", offset, port, receiver.cc_soundController2(channel, controlValue));
    }
    public OUTER cc_soundController3( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController3", offset, port, receiver.cc_soundController3(channel, controlValue));
    }
    public OUTER cc_soundController4( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController4", offset, port, receiver.cc_soundController4(channel, controlValue));
    }
    public OUTER cc_soundController5( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController5", offset, port, receiver.cc_soundController5(channel, controlValue));
    }
    public OUTER cc_soundController6( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController6", offset, port, receiver.cc_soundController6(channel, controlValue));
    }
    public OUTER cc_soundController7( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController7", offset, port, receiver.cc_soundController7(channel, controlValue));
    }
    public OUTER cc_soundController8( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController8", offset, port, receiver.cc_soundController8(channel, controlValue));
    }
    public OUTER cc_soundController9( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController9", offset, port, receiver.cc_soundController9(channel, controlValue));
    }
    public OUTER cc_soundController10( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_soundController10", offset, port, receiver.cc_soundController10(channel, controlValue));
    }
    public OUTER cc_generalPurpose01( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_generalPurpose01", offset, port, receiver.cc_generalPurpose01(channel, controlValue));
    }
    public OUTER cc_generalPurpose02( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_generalPurpose02", offset, port, receiver.cc_generalPurpose02(channel, controlValue));
    }
    public OUTER cc_generalPurpose03( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_generalPurpose03", offset, port, receiver.cc_generalPurpose03(channel, controlValue));
    }
    public OUTER cc_generalPurpose04( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_generalPurpose04", offset, port, receiver.cc_generalPurpose04(channel, controlValue));
    }
    public OUTER cc_portamento( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_portamento", offset, port, receiver.cc_portamento(channel, controlValue));
    }
    public OUTER cc_effect1( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_effect1", offset, port, receiver.cc_effect1(channel, controlValue));
    }
    public OUTER cc_effect2( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_effect2", offset, port, receiver.cc_effect2(channel, controlValue));
    }
    public OUTER cc_effect3( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_effect3", offset, port, receiver.cc_effect3(channel, controlValue));
    }
    public OUTER cc_effect4( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_effect4", offset, port, receiver.cc_effect4(channel, controlValue));
    }
    public OUTER cc_effect5( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_effect5", offset, port, receiver.cc_effect5(channel, controlValue));
    }
    public OUTER cc_dataIncrement( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_dataIncrement", offset, port, receiver.cc_dataIncrement(channel, controlValue));
    }
    public OUTER cc_dataDecrement( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_dataDecrement", offset, port, receiver.cc_dataDecrement(channel, controlValue));
    }
    public OUTER cc_nrpnLsb( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_nrpnLsb", offset, port, receiver.cc_nrpnLsb(channel, controlValue));
    }
    public OUTER cc_nrpnMsb( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_nrpnMsb", offset, port, receiver.cc_nrpnMsb(channel, controlValue));
    }
    public OUTER cc_rpnLsb( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_rpnLsb", offset, port, receiver.cc_rpnLsb(channel, controlValue));
    }
    public OUTER cc_rpnMsb( double offset, MetroPort port, int channel, int controlValue) {
        return this.convertResult( "cc_rpnMsb", offset, port, receiver.cc_rpnMsb(channel, controlValue));
    }
}

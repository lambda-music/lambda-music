package metro;

public abstract class MetroMidiReceiverBufferer<OUTER,INNER> implements MetroMidiReceiver<OUTER> {
    public static class Default<T> extends MetroMidiReceiverBufferer<T,T> {
        public Default(MetroBufferedMidiReceiver<T> receiver) {
            super( receiver );
        }
        @Override
        protected T convertResult(T result) {
            return result;
        }
    }
    public static <T> MetroMidiReceiverBufferer<T,T> createRecorder( MetroBufferedMidiReceiver<T> receiver ) {
        return new MetroMidiReceiverBufferer.Default<T>( receiver );
    }

    private final MetroBufferedMidiReceiver<INNER> receiver;
    public MetroMidiReceiverBufferer( MetroBufferedMidiReceiver<INNER> receiver ) {
        this.receiver = receiver;
    }
    private transient MetroPort port;
    private transient double offset;
    public MetroPort getPort() {
        return port;
    }
    public void setPort(MetroPort port) {
        this.port = port;
    }
    public double getOffset() {
        return offset;
    }
    public void setOffset(double offset) {
        this.offset = offset;
    }

    protected abstract OUTER convertResult( INNER result );
    
    private boolean endCalled=false;
    @Override
    public boolean endCalled() {
        return endCalled;
    }
    @Override
    public OUTER end() {
        this.endCalled = true;
        return convertResult( receiver.end() );
    }
    public OUTER error(String string) {
        return convertResult( receiver.error( offset, port, string ) );
    }
    public OUTER noteOn(int channel, int note, double velocity) {
        return convertResult( receiver.noteOn( offset, port, channel, note, velocity ));
    }
    public OUTER noteOn(int channel, int note, int velocity) {
        return convertResult( receiver.noteOn( offset, port, channel, note, velocity ));
    }
    public OUTER noteOff(int channel, int note, double velocity) {
        return convertResult( receiver.noteOff( offset, port, channel, note, velocity ));
    }
    public OUTER noteOff(int channel, int note, int velocity) {
        return convertResult( receiver.noteOff( offset, port, channel, note, velocity ));
    }
    public OUTER keyPressure(int channel, int note, double pressure) {
        return convertResult( receiver.keyPressure( offset, port, channel, note, pressure ));
    }
    public OUTER keyPressure(int channel, int note, int pressure) {
        return convertResult( receiver.keyPressure( offset, port, channel, note, pressure ));
    }
    public OUTER controlChange(int channel, int controlNumber, int controlValue) {
        return convertResult( receiver.controlChange( offset, port, channel, controlNumber, controlValue ));
    }
    public OUTER programChange(int ch, int programNumber) {
        return convertResult( receiver.programChange( offset, port, ch, programNumber ));
    }
    public OUTER channelPressure(int ch, double pressureValue) {
        return convertResult( receiver.channelPressure( offset, port, ch, pressureValue ));
    }
    public OUTER channelPressure(int ch, int pressureValue) {
        return convertResult( receiver.channelPressure( offset, port, ch, pressureValue ));
    }
    public OUTER pitchBend(int ch, double pitchBendValue) {
        return convertResult( receiver.pitchBend( offset, port, ch, pitchBendValue ));
    }
    public OUTER pitchBend(int ch, int pitchBendValue) {
        return convertResult( receiver.pitchBend( offset, port, ch, pitchBendValue ));
    }
    public OUTER cc_allSoundOff(int ch) {
        return convertResult( receiver.cc_allSoundOff( offset, port, ch ));
    }
    public OUTER cc_resetAllControllers(int ch) {
        return convertResult( receiver.cc_resetAllControllers( offset, port, ch ));
    }
    public OUTER cc_localControls(int ch, boolean on) {
        return convertResult( receiver.cc_localControls( offset, port, ch, on ));
    }
    public OUTER cc_allNoteOff(int ch) {
        return convertResult( receiver.cc_allNoteOff( offset, port, ch ));
    }
    public OUTER cc_omniModeOff(int ch) {
        return convertResult( receiver.cc_omniModeOff( offset, port, ch ));
    }
    public OUTER cc_omniModeOn(int ch) {
        return convertResult( receiver.cc_omniModeOn( offset, port, ch ));
    }
    public OUTER cc_monoModeOn(int ch) {
        return convertResult( receiver.cc_monoModeOn( offset, port, ch ));
    }
    public OUTER cc_polyModeOn(int ch) {
        return convertResult( receiver.cc_polyModeOn( offset, port, ch ));
    }
    public OUTER songPositionPointer(int pos) {
        return convertResult( receiver.songPositionPointer( offset, port, pos ));
    }
    public OUTER songSelect(int songNumber) {
        return convertResult( receiver.songSelect( offset, port, songNumber ));
    }
    public OUTER endOfExclusive() {
        return convertResult( receiver.endOfExclusive(offset, port));
    }
    public OUTER clock() {
        return convertResult( receiver.clock( offset, port ));
    }
    public OUTER start() {
        return convertResult( receiver.start( offset, port ));
    }
    public OUTER cont() {
        return convertResult( receiver.cont( offset, port ));
    }
    public OUTER stop() {
        return convertResult( receiver.stop( offset, port ));
    }
    public OUTER reset() {
        return convertResult( receiver.reset( offset, port ));
    }
    public OUTER cc_bankSelect(int channel, int value) {
        return convertResult( receiver.cc_bankSelect( offset, port, channel, value ));
    }
    public OUTER cc_modulation(int channel, int value) {
        return convertResult( receiver.cc_modulation( offset, port, channel, value ));
    }
    public OUTER cc_breathController(int channel, int value) {
        return convertResult( receiver.cc_breathController( offset, port, channel, value ));
    }
    public OUTER cc_footController(int channel, int value) {
        return convertResult( receiver.cc_footController( offset, port, channel, value ));
    }
    public OUTER cc_portamentoTime(int channel, int value) {
        return convertResult( receiver.cc_portamentoTime( offset, port, channel, value ));
    }
    public OUTER cc_dataEntryMsb(int channel, int value) {
        return convertResult( receiver.cc_dataEntryMsb( offset, port, channel, value ));
    }
    public OUTER cc_volume(int channel, int value) {
        return convertResult( receiver.cc_volume( offset, port, channel, value ));
    }
    public OUTER cc_balance(int channel, int value) {
        return convertResult( receiver.cc_balance( offset, port, channel, value ));
    }
    public OUTER cc_pan(int channel, int value) {
        return convertResult( receiver.cc_pan( offset, port, channel, value ));
    }
    public OUTER cc_expression(int channel, int value) {
        return convertResult( receiver.cc_expression( offset, port, channel, value ));
    }
    public OUTER cc_effectController1(int channel, int value) {
        return convertResult( receiver.cc_effectController1( offset, port, channel, value ));
    }
    public OUTER cc_effectController2(int channel, int value) {
        return convertResult( receiver.cc_effectController2( offset, port, channel, value ));
    }
    public OUTER cc_sustainPedal(int channel, int value) {
        return convertResult( receiver.cc_sustainPedal( offset, port, channel, value ));
    }
    public OUTER cc_portamentoSwitch(int channel, int value) {
        return convertResult( receiver.cc_portamentoSwitch( offset, port, channel, value ));
    }
    public OUTER cc_sostenutoSwitch(int channel, int value) {
        return convertResult( receiver.cc_sostenutoSwitch( offset, port, channel, value ));
    }
    public OUTER cc_pedalSwitch(int channel, int value) {
        return convertResult( receiver.cc_pedalSwitch( offset, port, channel, value ));
    }
    public OUTER cc_legatoSwitch(int channel, int value) {
        return convertResult( receiver.cc_legatoSwitch( offset, port, channel, value ));
    }
    public OUTER cc_hold2(int channel, int value) {
        return convertResult( receiver.cc_hold2( offset, port, channel, value ));
    }
    public OUTER cc_soundController1(int channel, int value) {
        return convertResult( receiver.cc_soundController1( offset, port, channel, value ));
    }
    public OUTER cc_soundController2(int channel, int value) {
        return convertResult( receiver.cc_soundController2( offset, port, channel, value ));
    }
    public OUTER cc_soundController3(int channel, int value) {
        return convertResult( receiver.cc_soundController3( offset, port, channel, value ));
    }
    public OUTER cc_soundController4(int channel, int value) {
        return convertResult( receiver.cc_soundController4( offset, port, channel, value ));
    }
    public OUTER cc_soundController5(int channel, int value) {
        return convertResult( receiver.cc_soundController5( offset, port, channel, value ));
    }
    public OUTER cc_soundController6(int channel, int value) {
        return convertResult( receiver.cc_soundController6( offset, port, channel, value ));
    }
    public OUTER cc_soundController7(int channel, int value) {
        return convertResult( receiver.cc_soundController7( offset, port, channel, value ));
    }
    public OUTER cc_soundController8(int channel, int value) {
        return convertResult( receiver.cc_soundController8( offset, port, channel, value ));
    }
    public OUTER cc_soundController9(int channel, int value) {
        return convertResult( receiver.cc_soundController9( offset, port, channel, value ));
    }
    public OUTER cc_soundController10(int channel, int value) {
        return convertResult( receiver.cc_soundController10( offset, port, channel, value ));
    }
    public OUTER cc_generalPurpose01(int channel, int value) {
        return convertResult( receiver.cc_generalPurpose01( offset, port, channel, value ));
    }
    public OUTER cc_generalPurpose02(int channel, int value) {
        return convertResult( receiver.cc_generalPurpose02( offset, port, channel, value ));
    }
    public OUTER cc_generalPurpose03(int channel, int value) {
        return convertResult( receiver.cc_generalPurpose03( offset, port, channel, value ));
    }
    public OUTER cc_generalPurpose04(int channel, int value) {
        return convertResult( receiver.cc_generalPurpose04( offset, port, channel, value ));
    }
    public OUTER cc_portamento(int channel, int value) {
        return convertResult( receiver.cc_portamento( offset, port, channel, value ));
    }
    public OUTER cc_effect1(int channel, int value) {
        return convertResult( receiver.cc_effect1( offset, port, channel, value ));
    }
    public OUTER cc_effect2(int channel, int value) {
        return convertResult( receiver.cc_effect2( offset, port, channel, value ));
    }
    public OUTER cc_effect3(int channel, int value) {
        return convertResult( receiver.cc_effect3( offset, port, channel, value ));
    }
    public OUTER cc_effect4(int channel, int value) {
        return convertResult( receiver.cc_effect4( offset, port, channel, value ));
    }
    public OUTER cc_effect5(int channel, int value) {
        return convertResult( receiver.cc_effect5( offset, port, channel, value ));
    }
    public OUTER cc_dataIncrement(int channel, int value) {
        return convertResult( receiver.cc_dataIncrement( offset, port, channel, value ));
    }
    public OUTER cc_dataDecrement(int channel, int value) {
        return convertResult( receiver.cc_dataDecrement( offset, port, channel, value ));
    }
    public OUTER cc_nrpnLsb(int channel, int value) {
        return convertResult( receiver.cc_nrpnLsb( offset, port, channel, value ));
    }
    public OUTER cc_nrpnMsb(int channel, int value) {
        return convertResult( receiver.cc_nrpnMsb( offset, port, channel, value ));
    }
    public OUTER cc_rpnLsb(int channel, int value) {
        return convertResult( receiver.cc_rpnLsb( offset, port, channel, value ));
    }
    public OUTER cc_rpnMsb(int channel, int value) {
        return convertResult( receiver.cc_rpnMsb( offset, port, channel, value ));
    }
}

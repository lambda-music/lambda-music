package metro;

public class DefaultMetroMidiReceiver<T> implements MetroMidiReceiver<T> {
    @Override
    public boolean endCalled() {
        return false;
    }

    @Override
    public void end(MetroCollector<T> result) {
    }

    @Override
    public void error(MetroCollector<T> result, String string) {
    }

    @Override
    public void noteOn(MetroCollector<T> result, int channel, int note, double velocity) {
    }

    @Override
    public void noteOn(MetroCollector<T> result, int channel, int note, int velocity) {
    }

    @Override
    public void noteOff(MetroCollector<T> result, int channel, int note, double velocity) {
    }

    @Override
    public void noteOff(MetroCollector<T> result, int channel, int note, int velocity) {
    }

    @Override
    public void keyPressure(MetroCollector<T> result, int channel, int note, double pressure) {
    }

    @Override
    public void keyPressure(MetroCollector<T> result, int channel, int note, int pressure) {
    }

    @Override
    public void controlChange(MetroCollector<T> result, int channel, int controlNumber, int controlValue) {
    }

    @Override
    public void programChange(MetroCollector<T> result, int ch, int programNumber) {
    }

    @Override
    public void channelPressure(MetroCollector<T> result, int ch, double pressureValue) {
    }

    @Override
    public void channelPressure(MetroCollector<T> result, int ch, int pressureValue) {
    }

    @Override
    public void pitchBend(MetroCollector<T> result, int ch, double pitchBendValue) {
    }

    @Override
    public void pitchBend(MetroCollector<T> result, int ch, int pitchBendValue) {
    }

    @Override
    public void cc_allSoundOff(MetroCollector<T> result, int ch) {
    }

    @Override
    public void cc_resetAllControllers(MetroCollector<T> result, int ch) {
    }

    @Override
    public void cc_localControls(MetroCollector<T> result, int ch, boolean on) {
    }

    @Override
    public void cc_allNoteOff(MetroCollector<T> result, int ch) {
    }

    @Override
    public void cc_omniModeOff(MetroCollector<T> result, int ch) {
    }

    @Override
    public void cc_omniModeOn(MetroCollector<T> result, int ch) {
    }

    @Override
    public void cc_monoModeOn(MetroCollector<T> result, int ch) {
    }

    @Override
    public void cc_polyModeOn(MetroCollector<T> result, int ch) {
    }

    @Override
    public void songPositionPointer(MetroCollector<T> result, int pos) {
    }

    @Override
    public void songSelect(MetroCollector<T> result, int songNumber) {
    }

    @Override
    public void endOfExclusive(MetroCollector<T> result) {
    }

    @Override
    public void clock(MetroCollector<T> result) {
    }

    @Override
    public void start(MetroCollector<T> result) {
    }

    @Override
    public void cont(MetroCollector<T> result) {
    }

    @Override
    public void stop(MetroCollector<T> result) {
    }

    @Override
    public void reset(MetroCollector<T> result) {
    }

    @Override
    public void cc_bankSelect(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_modulation(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_breathController(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_footController(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_portamentoTime(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_dataEntryMsb(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_volume(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_balance(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_pan(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_expression(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_effectController1(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_effectController2(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_sustainPedal(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_portamentoSwitch(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_sostenutoSwitch(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_pedalSwitch(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_legatoSwitch(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_hold2(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController1(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController2(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController3(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController4(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController5(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController6(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController7(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController8(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController9(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_soundController10(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_generalPurpose01(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_generalPurpose02(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_generalPurpose03(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_generalPurpose04(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_portamento(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_effect1(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_effect2(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_effect3(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_effect4(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_effect5(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_dataIncrement(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_dataDecrement(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_nrpnLsb(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_nrpnMsb(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_rpnLsb(MetroCollector<T> result, int channel, int value) {
    }

    @Override
    public void cc_rpnMsb(MetroCollector<T> result, int channel, int value) {
    }
}

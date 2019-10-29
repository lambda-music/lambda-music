package metro;

import java.util.Iterator;

public class MetroRecordingEventBuffer implements Iterable<MetroEvent>, MetroBufferedMidiReceiver {

    @Override
    public void noteOn(double offset, MetroPort port, int channel, int note, double velocity) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void noteOn(double offset, MetroPort port, int channel, int note, int velocity) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void noteOff(double offset, MetroPort port, int channel, int note, double velocity) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void noteOff(double offset, MetroPort port, int channel, int note, int velocity) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void keyPressure(double offset, MetroPort port, int channel, int note, double pressure) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void keyPressure(double offset, MetroPort port, int channel, int note, int pressure) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void controlChange(double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void programChange(double offset, MetroPort port, int ch, int programNumber) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void channelPressure(double offset, MetroPort port, int ch, double pressureValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void channelPressure(double offset, MetroPort port, int ch, int pressureValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void pitchBend(double offset, MetroPort port, int ch, double pitchBendValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void pitchBend(double offset, MetroPort port, int ch, int pitchBendValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_allSoundOff(double offset, MetroPort port, int ch) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_resetAllControllers(double offset, MetroPort port, int ch) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_localControls(double offset, MetroPort port, int ch, boolean on) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_allNoteOff(double offset, MetroPort port, int ch) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_omniModeOff(double offset, MetroPort port, int ch) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_omniModeOn(double offset, MetroPort port, int ch) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_monoModeOn(double offset, MetroPort port, int ch) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_polyModeOn(double offset, MetroPort port, int ch) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void songPositionPointer(double offset, MetroPort port, int pos) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void songSelect(double offset, MetroPort port, int songNumber) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void endOfExclusive(double offset, MetroPort port) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void clock(double offset, MetroPort port) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void start(double offset, MetroPort port) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cont(double offset, MetroPort port) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void stop(double offset, MetroPort port) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void reset(double offset, MetroPort port) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_bankSelect(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_modulation(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_breathController(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_footController(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_portamentoTime(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_dataEntryMsb(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_volume(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_balance(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_pan(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_expression(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_effectController1(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_effectController2(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_sustainPedal(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_portamentoSwitch(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_sostenutoSwitch(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_pedalSwitch(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_legatoSwitch(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_hold2(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController1(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController2(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController3(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController4(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController5(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController6(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController7(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController8(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController9(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_soundController10(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_generalPurpose01(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_generalPurpose02(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_generalPurpose03(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_generalPurpose04(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_portamento(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_effect1(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_effect2(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_effect3(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_effect4(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_effect5(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_dataIncrement(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_dataDecrement(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_nrpnLsb(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_nrpnMsb(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_rpnLsb(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void cc_rpnMsb(double offset, MetroPort port, int channel, int value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Iterator<MetroEvent> iterator() {
        // TODO Auto-generated method stub
        return null;
    }
    
}

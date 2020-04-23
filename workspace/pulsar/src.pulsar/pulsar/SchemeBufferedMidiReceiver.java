package pulsar;

import gnu.lists.LList;
import metro.MetroBufferedMidiReceiver;
import metro.MetroPort;

public class SchemeBufferedMidiReceiver implements MetroBufferedMidiReceiver<LList> {
    public static final SchemeBufferedMidiReceiver INSTANCE = new SchemeBufferedMidiReceiver();
    public static SchemeBufferedMidiReceiver getInstance() {
        return INSTANCE;
    }
    private SchemeBufferedMidiReceiver() {
    }
    // system
    private boolean endCalled=false;
    @Override
    public boolean endCalled() {
        return endCalled;
    }
    @Override
    public LList end() {
        this.endCalled = true;
        return null;
    }
    
    @Override
    public LList event(double offset, LList event) {
        // (Mon, 11 Nov 2019 10:48:35 +0900) currently this method is not used; only for future compatibilities.
        // return pulsar.PulsarSpecialNoteListParsers.PARSER_EVENT((Runnable) event );
        // This method cannot be achieved.
        return null;
    }
    @Override
    public LList exec(double offset, Runnable runnable) {
        // (Mon, 11 Nov 2019 10:48:35 +0900) currently this method is not used; only for future compatibilities.
        // This method cannot be achieved.
        // return pulsar.PulsarSpecialNoteListParsers.PARSER_EXEC.exec( offset, runnable );
        return null;
    }
    @Override
    public LList length(double length) {
        // (Mon, 11 Nov 2019 10:48:35 +0900) currently this method is not used; only for future compatibilities.
        return pulsar.PulsarSpecialNoteListParsers.PARSER_BAR.length( length );
    }
    
    @Override
    public LList error(double offset, MetroPort port, String message) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_ERROR.error(offset, port, message );
    }

    // basic
    @Override
    public LList noteOn(double offset, MetroPort port, int channel, int note, double velocity) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_ON.noteOn(offset, port, channel, note, velocity);
    }

    @Override
    public LList noteOn(double offset, MetroPort port, int channel, int note, int velocity) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_ON.noteOn(offset, port, channel, note, velocity);
    }

    @Override
    public LList noteOff(double offset, MetroPort port, int channel, int note, double velocity) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_OFF.noteOff(offset, port, channel, note, velocity);
    }

    @Override
    public LList noteOff(double offset, MetroPort port, int channel, int note, int velocity) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_OFF.noteOff(offset, port, channel, note, velocity);
    }

    @Override
    public LList keyPressure(double offset, MetroPort port, int channel, int note, double pressure) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_KEY_PRESSURE.keyPressure(offset, port, channel, note, pressure);
    }

    @Override
    public LList keyPressure(double offset, MetroPort port, int channel, int note, int pressure) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_KEY_PRESSURE.keyPressure(offset, port, channel, note, pressure);
    }

    @Override
    public LList controlChange(double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_CONTROL_CHANGE.controlChange(offset, port, channel, controlNumber, controlValue);
    }

    @Override
    public LList programChange(double offset, MetroPort port, int channel, int programNumber) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_PROGRAM_CHANGE.programChange(offset, port, channel, programNumber);
    }

    @Override
    public LList channelPressure(double offset, MetroPort port, int channel, double pressureValue) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_CHANNEL_PRESSURE.channelPressure(offset, port, channel, pressureValue);
    }

    @Override
    public LList channelPressure(double offset, MetroPort port, int channel, int pressureValue) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_CHANNEL_PRESSURE.channelPressure(offset, port, channel, pressureValue);
    }

    @Override
    public LList pitchBend(double offset, MetroPort port, int channel, double pitchBendValue) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_PITCH_BEND.pitchBend(offset, port, channel, pitchBendValue);
    }

    @Override
    public LList pitchBend(double offset, MetroPort port, int channel, int pitchBendValue) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_PITCH_BEND.pitchBend(offset, port, channel, pitchBendValue);
    }

    @Override
    public LList cc_allSoundOff(double offset, MetroPort port, int channel) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_ALL_SOUND_OFF.cc_allSoundOff(offset, port, channel);
    }

    @Override
    public LList cc_resetAllControllers(double offset, MetroPort port, int channel) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_RESET_ALL_CONTROLLERS.cc_resetAllControllers(offset, port, channel);
    }

    @Override
    public LList cc_localControls(double offset, MetroPort port, int channel, boolean on) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_LOCAL_CONTROLS.cc_localControls(offset, port, channel, on);
    }

    @Override
    public LList cc_allNoteOff(double offset, MetroPort port, int channel) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_ALL_NOTE_OFF.cc_allNoteOff(offset, port, channel);
    }

    @Override
    public LList cc_omniModeOff(double offset, MetroPort port, int channel) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_OMNI_MODE_OFF.cc_omniModeOff(offset, port, channel);
    }

    @Override
    public LList cc_omniModeOn(double offset, MetroPort port, int channel) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_OMNI_MODE_ON.cc_omniModeOn(offset, port, channel);
    }

    @Override
    public LList cc_monoModeOn(double offset, MetroPort port, int channel) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_MONO_MODE_OFF.cc_monoModeOn(offset, port, channel);
    }

    @Override
    public LList cc_polyModeOn(double offset, MetroPort port, int channel) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_POLY_MODE_ON.cc_polyModeOn(offset, port, channel);
    }

    @Override
    public LList songPositionPointer(double offset, MetroPort port, int pos) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SONG_POSITION_POINTER.songPositionPointer(offset, port, pos);
    }

    @Override
    public LList songSelect(double offset, MetroPort port, int songNumber) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SONG_SELECT.songSelect(offset, port, songNumber);
    }

    @Override
    public LList endOfExclusive(double offset, MetroPort port) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_END_OF_EXCLUSIVE.endOfExclusive(offset, port);
    }

    @Override
    public LList clock(double offset, MetroPort port) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_CLOCK.clock(offset, port);
    }

    @Override
    public LList start(double offset, MetroPort port) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_START.start(offset, port);
    }

    @Override
    public LList cont(double offset, MetroPort port) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_CONTINUE.cont(offset, port);
    }

    @Override
    public LList stop(double offset, MetroPort port) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_STOP.stop(offset, port);
    }

    @Override
    public LList reset(double offset, MetroPort port) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_RESET.reset(offset, port);
    }

    @Override
    public LList cc_bankSelect(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_BANK_SELECT.cc_bankSelect(offset, port, channel, value);
    }

    @Override
    public LList cc_modulation(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_MODULATION.cc_modulation(offset, port, channel, value);
    }

    @Override
    public LList cc_breathController(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_BREATH_CTRL.cc_breathController(offset, port, channel, value);
    }

    @Override
    public LList cc_footController(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_FOOT_CTRL.cc_footController(offset, port, channel, value);
    }

    @Override
    public LList cc_portamentoTime(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_PORTAMENTO_TIME.cc_portamentoTime(offset, port, channel, value);
    }

    @Override
    public LList cc_dataEntryMsb(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_DATA_ENTRY_MSB.cc_dataEntryMsb(offset, port, channel, value);
    }

    @Override
    public LList cc_volume(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_VOLUME.cc_volume(offset, port, channel, value);
    }

    @Override
    public LList cc_balance(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_BALANCE.cc_balance(offset, port, channel, value);
    }

    @Override
    public LList cc_pan(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_PAN.cc_pan(offset, port, channel, value);
    }

    @Override
    public LList cc_expression(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EXPRESSION.cc_expression(offset, port, channel, value);
    }

    @Override
    public LList cc_effectController1(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_CTRL_1.cc_effectController1(offset, port, channel, value);
    }

    @Override
    public LList cc_effectController2(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_CTRL_2.cc_effectController2(offset, port, channel, value);
    }

    @Override
    public LList cc_sustainPedal(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SUSTAIN_PEDAL.cc_sustainPedal(offset, port, channel, value);
    }

    @Override
    public LList cc_portamentoSwitch(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_PORTAMENTO_SWITCH.cc_portamentoSwitch(offset, port, channel, value);
    }

    @Override
    public LList cc_sostenutoSwitch(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOSTENUTO_SWITCH.cc_sostenutoSwitch(offset, port, channel, value);
    }

    @Override
    public LList cc_pedalSwitch(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOFT_PEDAL_SWITCH.cc_pedalSwitch(offset, port, channel, value);
    }

    @Override
    public LList cc_legatoSwitch(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_LEGATO_FOOTSWITCH.cc_legatoSwitch(offset, port, channel, value);
    }

    @Override
    public LList cc_hold2(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_HOLD_2.cc_hold2(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController1(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_01.cc_soundController1(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController2(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_02.cc_soundController2(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController3(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_03.cc_soundController3(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController4(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_04.cc_soundController4(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController5(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_05.cc_soundController5(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController6(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_06.cc_soundController6(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController7(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_07.cc_soundController7(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController8(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_08.cc_soundController8(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController9(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_09.cc_soundController9(offset, port, channel, value);
    }

    @Override
    public LList cc_soundController10(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_10.cc_soundController10(offset, port, channel, value);
    }

    @Override
    public LList cc_generalPurpose01(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_01.cc_generalPurpose01(offset, port, channel, value);
    }

    @Override
    public LList cc_generalPurpose02(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_02.cc_generalPurpose02(offset, port, channel, value);
    }

    @Override
    public LList cc_generalPurpose03(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_03.cc_generalPurpose03(offset, port, channel, value);
    }

    @Override
    public LList cc_generalPurpose04(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_04.cc_generalPurpose04(offset, port, channel, value);
    }

    @Override
    public LList cc_portamento(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_PORTAMENTO_CC_CTRL.cc_portamento(offset, port, channel, value);
    }

    @Override
    public LList cc_effect1(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_1_DEPTH.cc_effect1(offset, port, channel, value);
    }

    @Override
    public LList cc_effect2(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_2_DEPTH.cc_effect2(offset, port, channel, value);
    }

    @Override
    public LList cc_effect3(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_3_DEPTH.cc_effect3(offset, port, channel, value);
    }

    @Override
    public LList cc_effect4(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_4_DEPTH.cc_effect4(offset, port, channel, value);
    }

    @Override
    public LList cc_effect5(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_5_DEPTH.cc_effect5(offset, port, channel, value);
    }

    @Override
    public LList cc_dataIncrement(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_DATA_INCREMENT.cc_dataIncrement(offset, port, channel, value);
    }

    @Override
    public LList cc_dataDecrement(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_DATA_DECREMENT.cc_dataDecrement(offset, port, channel, value);
    }

    @Override
    public LList cc_nrpnLsb(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_NRPN_LSB.cc_nrpnLsb(offset, port, channel, value);
    }

    @Override
    public LList cc_nrpnMsb(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_NRPN_MSB.cc_nrpnMsb(offset, port, channel, value);
    }

    @Override
    public LList cc_rpnLsb(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_RPN_LSB.cc_rpnLsb(offset, port, channel, value);
    }

    @Override
    public LList cc_rpnMsb(double offset, MetroPort port, int channel, int value) {
        return pulsar.PulsarMidiNoteListParsers.PARSER_RPN_MSB.cc_rpnMsb(offset, port, channel, value);
    }
}

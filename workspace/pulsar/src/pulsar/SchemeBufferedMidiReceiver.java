package pulsar;

import gnu.lists.LList;
import metro.MetroBufferedMidiReceiver;
import metro.MetroPort;

public class SchemeBufferedMidiReceiver implements MetroBufferedMidiReceiver<LList> {
    @Override
    public LList error(double offset, MetroPort port, String message) {
        // NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_ON;
        return null;
    }

    @Override
    public LList noteOn(double offset, MetroPort port, int channel, int note, double velocity) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_ON;
        return null;
    }

    @Override
    public LList noteOn(double offset, MetroPort port, int channel, int note, int velocity) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_ON;
        return null;
    }

    @Override
    public LList noteOff(double offset, MetroPort port, int channel, int note, double velocity) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_OFF;
        return null;
    }

    @Override
    public LList noteOff(double offset, MetroPort port, int channel, int note, int velocity) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_NOTE_OFF;
        return null;
    }

    @Override
    public LList keyPressure(double offset, MetroPort port, int channel, int note, double pressure) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_KEY_PRESSURE;
        return null;
    }

    @Override
    public LList keyPressure(double offset, MetroPort port, int channel, int note, int pressure) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_KEY_PRESSURE;
        return null;
    }

    @Override
    public LList controlChange(double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_CONTROL_CHANGE;
        return null;
    }

    @Override
    public LList programChange(double offset, MetroPort port, int ch, int programNumber) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_PROGRAM_CHANGE;
        return null;
    }

    @Override
    public LList channelPressure(double offset, MetroPort port, int ch, double pressureValue) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_CHANNEL_PRESSURE;
        return null;
    }

    @Override
    public LList channelPressure(double offset, MetroPort port, int ch, int pressureValue) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_CHANNEL_PRESSURE;
        return null;
    }

    @Override
    public LList pitchBend(double offset, MetroPort port, int ch, double pitchBendValue) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_PITCH_BEND;
        return null;
    }

    @Override
    public LList pitchBend(double offset, MetroPort port, int ch, int pitchBendValue) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_PITCH_BEND;
        return null;
    }

    @Override
    public LList cc_allSoundOff(double offset, MetroPort port, int ch) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_ALL_SOUND_OFF;
        return null;
    }

    @Override
    public LList cc_resetAllControllers(double offset, MetroPort port, int ch) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_RESET_ALL_CONTROLLERS;
        return null;
    }

    @Override
    public LList cc_localControls(double offset, MetroPort port, int ch, boolean on) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_LOCAL_CONTROLS;
        return null;
    }

    @Override
    public LList cc_allNoteOff(double offset, MetroPort port, int ch) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_ALL_NOTE_OFF;
        return null;
    }

    @Override
    public LList cc_omniModeOff(double offset, MetroPort port, int ch) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_OMNI_MODE_OFF;
        return null;
    }

    @Override
    public LList cc_omniModeOn(double offset, MetroPort port, int ch) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_OMNI_MODE_ON;
        return null;
    }

    @Override
    public LList cc_monoModeOn(double offset, MetroPort port, int ch) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_MONO_MODE_OFF;
        return null;
    }

    @Override
    public LList cc_polyModeOn(double offset, MetroPort port, int ch) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_POLY_MODE_ON;
        return null;
    }

    @Override
    public LList songPositionPointer(double offset, MetroPort port, int pos) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SONG_POSITION_POINTER;
        return null;
    }

    @Override
    public LList songSelect(double offset, MetroPort port, int songNumber) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SONG_SELECT;
        return null;
    }

    @Override
    public LList endOfExclusive(double offset, MetroPort port) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_END_OF_EXCLUSIVE;
        return null;
    }

    @Override
    public LList clock(double offset, MetroPort port) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_CLOCK;
        return null;
    }

    @Override
    public LList start(double offset, MetroPort port) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_START;
        return null;
    }

    @Override
    public LList cont(double offset, MetroPort port) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_CONTINUE;
        return null;
    }

    @Override
    public LList stop(double offset, MetroPort port) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_STOP;
        return null;
    }

    @Override
    public LList reset(double offset, MetroPort port) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_RESET;
        return null;
    }

    @Override
    public LList cc_bankSelect(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_BANK_SELECT;
        return null;
    }

    @Override
    public LList cc_modulation(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_MODULATION;
        return null;
    }

    @Override
    public LList cc_breathController(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_BREATH_CTRL;
        return null;
    }

    @Override
    public LList cc_footController(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_FOOT_CTRL;
        return null;
    }

    @Override
    public LList cc_portamentoTime(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_PORTAMENTO_TIME;
        return null;
    }

    @Override
    public LList cc_dataEntryMsb(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_DATA_ENTRY_MSB;
        return null;
    }

    @Override
    public LList cc_volume(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_VOLUME;
        return null;
    }

    @Override
    public LList cc_balance(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_BALANCE;
        return null;
    }

    @Override
    public LList cc_pan(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_PAN;
        return null;
    }

    @Override
    public LList cc_expression(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EXPRESSION;
        return null;
    }

    @Override
    public LList cc_effectController1(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_CTRL_1;
        return null;
    }

    @Override
    public LList cc_effectController2(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_CTRL_2;
        return null;
    }

    @Override
    public LList cc_sustainPedal(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SUSTAIN_PEDAL;
        return null;
    }

    @Override
    public LList cc_portamentoSwitch(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_PORTAMENTO_SWITCH;
        return null;
    }

    @Override
    public LList cc_sostenutoSwitch(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOSTENUTO_SWITCH;
        return null;
    }

    @Override
    public LList cc_pedalSwitch(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOFT_PEDAL_SWITCH;
        return null;
    }

    @Override
    public LList cc_legatoSwitch(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_LEGATO_FOOTSWITCH;
        return null;
    }

    @Override
    public LList cc_hold2(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_HOLD_2;
        return null;
    }

    @Override
    public LList cc_soundController1(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_01;
        return null;
    }

    @Override
    public LList cc_soundController2(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_02;
        return null;
    }

    @Override
    public LList cc_soundController3(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_03;
        return null;
    }

    @Override
    public LList cc_soundController4(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_04;
        return null;
    }

    @Override
    public LList cc_soundController5(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_05;
        return null;
    }

    @Override
    public LList cc_soundController6(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_06;
        return null;
    }

    @Override
    public LList cc_soundController7(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_07;
        return null;
    }

    @Override
    public LList cc_soundController8(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_08;
        return null;
    }

    @Override
    public LList cc_soundController9(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_09;
        return null;
    }

    @Override
    public LList cc_soundController10(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_SOUND_CTRL_10;
        return null;
    }

    @Override
    public LList cc_generalPurpose01(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_01;
        return null;
    }

    @Override
    public LList cc_generalPurpose02(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_02;
        return null;
    }

    @Override
    public LList cc_generalPurpose03(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_03;
        return null;
    }

    @Override
    public LList cc_generalPurpose04(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_GENERAL_PURPOSE_04;
        return null;
    }

    @Override
    public LList cc_portamento(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_PORTAMENTO_CC_CTRL;
        return null;
    }

    @Override
    public LList cc_effect1(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_1_DEPTH;
        return null;
    }

    @Override
    public LList cc_effect2(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_2_DEPTH;
        return null;
    }

    @Override
    public LList cc_effect3(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_3_DEPTH;
        return null;
    }

    @Override
    public LList cc_effect4(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_4_DEPTH;
        return null;
    }

    @Override
    public LList cc_effect5(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_EFFECT_5_DEPTH;
        return null;
    }

    @Override
    public LList cc_dataIncrement(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_DATA_INCREMENT;
        return null;
    }

    @Override
    public LList cc_dataDecrement(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_DATA_DECREMENT;
        return null;
    }

    @Override
    public LList cc_nrpnLsb(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_NRPN_LSB;
        return null;
    }

    @Override
    public LList cc_nrpnMsb(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_NRPN_MSB;
        return null;
    }

    @Override
    public LList cc_rpnLsb(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_RPN_LSB;
        return null;
    }

    @Override
    public LList cc_rpnMsb(double offset, MetroPort port, int channel, int value) {
        NoteListParserElement element = pulsar.PulsarMidiNoteListParsers.PARSER_RPN_MSB;
        return null;
    }
    
}

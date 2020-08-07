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

/**
 * 
 * This class is often used with the medhod {@link MetroMidiMessages#receive(MetroMidiReceiver, byte[])}.
 * See the {@link MetroMidiMessages} class.
 * 
 * There are two type of Midi receivers: {@link MetroBufferedMidiReceiver} and this {@link MetroMidiReceiver}.
 * Both two classes are often intercahngablly converted to one another. See {@link MetroBufferedToDirectMidiReceiver} and
 * {@link MetroDirectToBufferedMidiReceiver}.
 * 
 * @author Ats Oka
 * @param <T>
 */
public interface MetroMidiReceiver<T> {
    public default T voidValue( String message ) {
        return null;
    };
    
    // special
    public boolean endCalled();
    public void end( MetroCollector<T> result );
    public void error(MetroCollector<T> result, String string );
    
    // basic
    public void noteOn(MetroCollector<T> result, int channel, int note, double velocity );
    public void noteOn(MetroCollector<T> result, int channel, int note, int velocity );
    public void noteOff(MetroCollector<T> result, int channel, int note, double velocity);
    public void noteOff(MetroCollector<T> result, int channel, int note, int velocity );
    public void keyPressure(MetroCollector<T> result, int channel, int note, double pressure);
    public void keyPressure(MetroCollector<T> result, int channel, int note, int pressure);
    public void controlChange(MetroCollector<T> result, int channel, int controlNumber, int controlValue );
    public void programChange(MetroCollector<T> result, int ch, int programNumber );
    public void channelPressure(MetroCollector<T> result, int ch, double pressureValue );
    public void channelPressure(MetroCollector<T> result, int ch, int pressureValue );
    public void pitchBend(MetroCollector<T> result, int ch, double pitchBendValue );
    public void pitchBend(MetroCollector<T> result, int ch, int pitchBendValue );

    public void cc_allSoundOff(MetroCollector<T> result, int ch ); 
    public void cc_resetAllControllers(MetroCollector<T> result, int ch ); 
    public void cc_localControls(MetroCollector<T> result, int ch, boolean on ); 
    public void cc_allNoteOff(MetroCollector<T> result, int ch ); 
    public void cc_omniModeOff(MetroCollector<T> result, int ch ); 
    public void cc_omniModeOn(MetroCollector<T> result, int ch ); 
    public void cc_monoModeOn(MetroCollector<T> result, int ch ); 
    public void cc_polyModeOn(MetroCollector<T> result, int ch ); 

    // system
    public void songPositionPointer(MetroCollector<T> result, int pos ); 
    public void songSelect(MetroCollector<T> result, int songNumber ); 
    public void endOfExclusive(MetroCollector<T> result ); 
    public void clock(MetroCollector<T> result ); 
    public void start(MetroCollector<T> result ); 
    public void cont(MetroCollector<T> result ); 
    public void stop(MetroCollector<T> result ); 
    public void reset(MetroCollector<T> result );
    
    // control change
    public void cc_bankSelect(MetroCollector<T> result, int channel, int value ) ;
    public void cc_modulation(MetroCollector<T> result, int channel, int value ) ;
    public void cc_breathController(MetroCollector<T> result, int channel, int value ) ;
    public void cc_footController(MetroCollector<T> result, int channel, int value ) ;
    public void cc_portamentoTime(MetroCollector<T> result, int channel, int value ) ;
    public void cc_dataEntryMsb(MetroCollector<T> result, int channel, int value ) ;
    public void cc_volume(MetroCollector<T> result, int channel, int value ) ;
    public void cc_balance(MetroCollector<T> result, int channel, int value ) ;
    public void cc_pan(MetroCollector<T> result, int channel, int value ) ;
    public void cc_expression(MetroCollector<T> result, int channel, int value ) ;
    public void cc_effectController1(MetroCollector<T> result, int channel, int value ) ;
    public void cc_effectController2(MetroCollector<T> result, int channel, int value ) ;
    public void cc_sustainPedal(MetroCollector<T> result, int channel, int value ) ;
    public void cc_portamentoSwitch(MetroCollector<T> result, int channel, int value ) ;
    public void cc_sostenutoSwitch(MetroCollector<T> result, int channel, int value ) ;
    public void cc_pedalSwitch(MetroCollector<T> result, int channel, int value ) ;
    public void cc_legatoSwitch(MetroCollector<T> result, int channel, int value ) ;
    public void cc_hold2(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController1(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController2(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController3(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController4(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController5(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController6(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController7(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController8(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController9(MetroCollector<T> result, int channel, int value ) ;
    public void cc_soundController10(MetroCollector<T> result, int channel, int value ) ;
    public void cc_generalPurpose01(MetroCollector<T> result, int channel, int value ) ;
    public void cc_generalPurpose02(MetroCollector<T> result, int channel, int value ) ;
    public void cc_generalPurpose03(MetroCollector<T> result, int channel, int value ) ;
    public void cc_generalPurpose04(MetroCollector<T> result, int channel, int value ) ;
    public void cc_portamento(MetroCollector<T> result, int channel, int value ) ;
    public void cc_effect1(MetroCollector<T> result, int channel, int value ) ;
    public void cc_effect2(MetroCollector<T> result, int channel, int value ) ;
    public void cc_effect3(MetroCollector<T> result, int channel, int value ) ;
    public void cc_effect4(MetroCollector<T> result, int channel, int value ) ;
    public void cc_effect5(MetroCollector<T> result, int channel, int value ) ;
    public void cc_dataIncrement(MetroCollector<T> result, int channel, int value ) ;
    public void cc_dataDecrement(MetroCollector<T> result, int channel, int value ) ;
    public void cc_nrpnLsb(MetroCollector<T> result, int channel, int value ) ;
    public void cc_nrpnMsb(MetroCollector<T> result, int channel, int value ) ;
    public void cc_rpnLsb(MetroCollector<T> result, int channel, int value ) ;
    public void cc_rpnMsb(MetroCollector<T> result, int channel, int value ) ;
    
    static abstract class Default<T> implements MetroMidiReceiver<T> {
        protected abstract T defaultValue();
        private boolean endCalled=false;
        @Override
        public boolean endCalled() {
            return endCalled;
        }
        @Override
        public void end(MetroCollector<T> result) {
            this.endCalled = true;
            result.collect( voidValue( "end" ));
        }
        @Override
        public void error(MetroCollector<T> result, String string ) {
            result.collect( voidValue( string ));
        }
        @Override
        public void noteOn(MetroCollector<T> result,int channel, int note, int velocity) {
            result.collect( defaultValue());
        }
        @Override
        public void noteOff(MetroCollector<T> result,int channel, int note, int velocity) {
            result.collect( defaultValue());
        }
        @Override
        public void keyPressure(MetroCollector<T> result,int channel, int note, int pressure) {
            result.collect( defaultValue());
        }
        @Override
        public void controlChange(MetroCollector<T> result,int channel, int controlNumber, int controlValue) {
            result.collect( defaultValue());
        }
        @Override
        public void programChange(MetroCollector<T> result,int ch, int programNumber) {
            result.collect( defaultValue());
        }
        @Override
        public void channelPressure(MetroCollector<T> result,int ch, int pressureValue) {
            result.collect( defaultValue());
        }
        @Override
        public void pitchBend(MetroCollector<T> result,int ch, int pitchBendValue) {
            result.collect( defaultValue());
        }
        @Override
        public void noteOn(MetroCollector<T> result,int channel, int note, double velocity) {
            result.collect( defaultValue());
        }
        @Override
        public void noteOff(MetroCollector<T> result,int channel, int note, double velocity) {
            result.collect( defaultValue());
        }
        @Override
        public void keyPressure(MetroCollector<T> result,int channel, int note, double pressure) {
            result.collect( defaultValue());
        }
        @Override
        public void channelPressure(MetroCollector<T> result,int ch, double pressureValue) {
            result.collect( defaultValue());
        }
        @Override
        public void pitchBend(MetroCollector<T> result,int ch, double pitchBendValue) {
            result.collect( defaultValue());
        }

        
        @Override
        public void cc_allSoundOff(MetroCollector<T> result,int ch) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_resetAllControllers(MetroCollector<T> result,int ch) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_localControls(MetroCollector<T> result,int ch, boolean on) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_allNoteOff(MetroCollector<T> result,int ch) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_omniModeOff(MetroCollector<T> result,int ch) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_omniModeOn(MetroCollector<T> result,int ch) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_monoModeOn(MetroCollector<T> result,int ch) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_polyModeOn(MetroCollector<T> result,int ch) {
            result.collect( defaultValue());
        }
        @Override
        public void songPositionPointer(MetroCollector<T> result,int pos) {
            result.collect( defaultValue());
        }
        @Override
        public void songSelect(MetroCollector<T> result,int songNumber) {
            result.collect( defaultValue());
        }
        @Override
        public void endOfExclusive(MetroCollector<T> result) {
            result.collect( defaultValue());
        }
        @Override
        public void clock(MetroCollector<T> result) {
            result.collect( defaultValue());
        }
        @Override
        public void start(MetroCollector<T> result) {
            result.collect( defaultValue());
        }
        @Override
        public void cont(MetroCollector<T> result) {
            result.collect( defaultValue());
        }
        @Override
        public void stop(MetroCollector<T> result) {
            result.collect( defaultValue());
        }
        @Override
        public void reset(MetroCollector<T> result) {
            result.collect( defaultValue());
        }

        @Override
        public void cc_bankSelect(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_modulation(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_breathController(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_footController(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_portamentoTime(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_dataEntryMsb(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_volume(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_balance(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_pan(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_expression(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_effectController1(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_effectController2(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_sustainPedal(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_portamentoSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_sostenutoSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_pedalSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_legatoSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_hold2(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController1(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController2(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController3(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController4(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController5(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController6(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController7(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController8(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController9(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_soundController10(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_generalPurpose01(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_generalPurpose02(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_generalPurpose03(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_generalPurpose04(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_portamento(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_effect1(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_effect2(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_effect3(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_effect4(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_effect5(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_dataIncrement(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_dataDecrement(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_nrpnLsb(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_nrpnMsb(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_rpnLsb(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
        @Override
        public void cc_rpnMsb(MetroCollector<T> result, int channel, int value ) {
            result.collect( defaultValue());
        }
    }
    
    static abstract class Formatter<T> implements MetroMidiReceiver<T> {
        protected abstract T format( String method, String params, Object ... args );
        private boolean endCalled=false;
        @Override
        public boolean endCalled() {
            return endCalled;
        }
        @Override
        public void end(MetroCollector<T> result) {
            this.endCalled = true;
            result.collect(format("end" , "" ));
        }
        @Override
        public void error(MetroCollector<T> result,String string) {
            result.collect(format("error" , "String string", string ));
        }
        @Override
        public void noteOn(MetroCollector<T> result,int channel, int note, int velocity) {
            result.collect(format( "noteOn", "int channel, int note, int velocity",channel,note,velocity ));
        }
        @Override
        public void noteOff(MetroCollector<T> result,int channel, int note, int velocity) {
            result.collect(format( "noteOff", "int channel, int note, int velocity",channel,note,velocity ));
        }
        @Override
        public void keyPressure(MetroCollector<T> result,int channel, int note, int pressure) {
            result.collect(format( "keyPressure", "int channel, int note, int pressure",channel,note,pressure ));
        }
        @Override
        public void controlChange(MetroCollector<T> result,int channel, int controlNumber, int controlValue) {
            result.collect(format( "controlChange", "int channel, int controlNumber, int controlValue",channel,controlNumber,controlValue ));
        }
        @Override
        public void programChange(MetroCollector<T> result,int ch, int programNumber) {
            result.collect(format( "programChange", "int ch, int programNumber",ch,programNumber ));
        }
        @Override
        public void channelPressure(MetroCollector<T> result,int ch, int pressureValue) {
            result.collect(format( "channelPressure", "int ch, int pressureValue",ch,pressureValue ));
        }
        @Override
        public void pitchBend(MetroCollector<T> result,int ch, int pitchBendValue) {
            result.collect(format( "pitchBend", "int ch, int pitchBendValue",ch,pitchBendValue ));
        }
        @Override
        public void noteOn(MetroCollector<T> result,int channel, int note, double velocity) {
            result.collect(format( "noteOn", "int channel, int note, double velocity",channel,note,velocity ));
        }
        @Override
        public void noteOff(MetroCollector<T> result,int channel, int note, double velocity) {
            result.collect(format( "noteOff", "int channel, int note, double velocity",channel,note,velocity ));
        }
        @Override
        public void keyPressure(MetroCollector<T> result,int channel, int note, double pressure) {
            result.collect(format( "keyPressure", "int channel, int note, double pressure",channel,note,pressure ));
        }
        @Override
        public void channelPressure(MetroCollector<T> result,int ch, double pressureValue) {
            result.collect(format( "channelPressure", "int ch, double pressureValue",ch,pressureValue ));
        }
        @Override
        public void pitchBend(MetroCollector<T> result,int ch, double pitchBendValue) {
            result.collect(format( "pitchBend", "int ch, double pitchBendValue",ch,pitchBendValue ));
        }

        
        @Override
        public void cc_allSoundOff(MetroCollector<T> result,int ch) {
            result.collect(format( "cc_allSoundOff", "int ch",ch ));
        }
        @Override
        public void cc_resetAllControllers(MetroCollector<T> result,int ch) {
            result.collect(format( "cc_resetAllControllers", "int ch",ch ));
        }
        @Override
        public void cc_localControls(MetroCollector<T> result,int ch, boolean on) {
            result.collect(format( "cc_localControls", "int ch, boolean on",ch,on ));
        }
        @Override
        public void cc_allNoteOff(MetroCollector<T> result,int ch) {
            result.collect(format( "cc_allNoteOff", "int ch",ch ));
        }
        @Override
        public void cc_omniModeOff(MetroCollector<T> result,int ch) {
            result.collect(format( "cc_omniModeOff", "int ch",ch ));
        }
        @Override
        public void cc_omniModeOn(MetroCollector<T> result,int ch) {
            result.collect(format( "cc_omniModeOn", "int ch",ch ));
        }
        @Override
        public void cc_monoModeOn(MetroCollector<T> result,int ch) {
            result.collect(format( "cc_monoModeOn", "int ch",ch ));
        }
        @Override
        public void cc_polyModeOn(MetroCollector<T> result,int ch) {
            result.collect(format( "cc_polyModeOn", "int ch",ch ));
        }
        @Override
        public void songPositionPointer(MetroCollector<T> result,int pos) {
            result.collect(format( "songPositionPointer", "int pos",pos ));
        }
        @Override
        public void songSelect(MetroCollector<T> result,int songNumber) {
            result.collect(format( "songSelect", "int songNumber",songNumber ));
        }
        @Override
        public void endOfExclusive(MetroCollector<T> result) {
            result.collect(format( "endOfExclusive", "" ));
        }
        @Override
        public void clock(MetroCollector<T> result) {
            result.collect(format( "clock", "" ));
        }
        @Override
        public void start(MetroCollector<T> result) {
            result.collect(format( "start", "" ));
        }
        @Override
        public void cont(MetroCollector<T> result) {
            result.collect(format( "cont", "" ));
        }
        @Override
        public void stop(MetroCollector<T> result) {
            result.collect(format( "stop", "" ));
        }
        @Override
        public void reset(MetroCollector<T> result) {
            result.collect(format( "reset", "" ));
        }

        @Override
        public void cc_bankSelect(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_bankSelect", " int channel, int value ", channel,value  ));
        }
        @Override
        public void cc_modulation(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_modulation", " int channel, int value ", channel,value  ));
        }
        @Override
        public void cc_breathController(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_breathController", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_footController(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_footController", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_portamentoTime(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_portamentoTime", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_dataEntryMsb(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_dataEntryMsb", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_volume(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_volume", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_balance(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_balance", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_pan(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_pan", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_expression(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_expression", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_effectController1(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_effectController1", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_effectController2(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_effectController2", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_sustainPedal(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_sustainPedal", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_portamentoSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_portamentoSwitch", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_sostenutoSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_sostenutoSwitch", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_pedalSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_pedalSwitch", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_legatoSwitch(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_legatoSwitch", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_hold2(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_hold2", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController1(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController1", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController2(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController2", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController3(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController3", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController4(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController4", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController5(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController5", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController6(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController6", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController7(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController7", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController8(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController8", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController9(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController9", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_soundController10(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_soundController10", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_generalPurpose01(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_generalPurpose01", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_generalPurpose02(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_generalPurpose02", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_generalPurpose03(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_generalPurpose03", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_generalPurpose04(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_generalPurpose04", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_portamento(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_portamento", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_effect1(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_effect1", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_effect2(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_effect2", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_effect3(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_effect3", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_effect4(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_effect4", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_effect5(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_effect5", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_dataIncrement(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_dataIncrement", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_dataDecrement(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_dataDecrement", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_nrpnLsb(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_nrpnLsb", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_nrpnMsb(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_nrpnMsb", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_rpnLsb(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_rpnLsb", " int channel, int value ",channel,value  ));
        }
        @Override
        public void cc_rpnMsb(MetroCollector<T> result, int channel, int value ) {
            result.collect(format( "cc_rpnMsb", " int channel, int value ", channel, value  ));
        }
    }
    public static class FormatString extends Formatter<String> {
        private static final FormatString INSTANCE = new FormatString();
        public static FormatString getInstance() {
            return INSTANCE;
        }
        @Override
        protected String format(String method, String params, Object... args) {
            StringBuilder sb = new StringBuilder();
            for ( int i=0; i<args.length; i++ ) {
                sb.append( "%3s , " );
            }
            if ( sb.length() != 0 ) 
                sb.setLength( sb.length() - 3 );
            
            String format = sb.toString();
            return method + "(" + params + ") " + String.format( format, args );
        }
    }
}

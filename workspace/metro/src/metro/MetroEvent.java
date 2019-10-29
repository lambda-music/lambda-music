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

import java.util.Comparator;
import java.util.List;

/**
 * This interface provides common methods for objects that represents every
 * notes in a bar. Bars are usually generated by the tracks. This interface is
 * strongly related to {@link MetroTrack#progressCursor(int, List) } method;
 * refer the {@linkplain MetroTrack#progressCursor(int, List) method} for
 * further information.
 * <p>
 * These methods are called as a callback of JACKAudio processing; these method
 * should return as soon as possible. The heavy processing that blocks for
 * longer time than the current setting of JACK's frame rate causes JACK to
 * XRUN.
 * 
 * @author Ats Oka
 */
public interface MetroEvent {
    public static final Comparator<MetroEvent> BAR_OFFSET_COMPARATOR = new Comparator<MetroEvent>() {
        @Override
        public int compare( MetroEvent o1, MetroEvent o2) {
            int i;
            i = (int) Math.signum( o1.getBarOffset() - o2.getBarOffset() );
            if (i != 0 )
                return i;
            
            if ( o1 instanceof MetroMidiEvent &&  o2 instanceof MetroMidiEvent ) {
                MetroMidiEvent mo1 = (MetroMidiEvent) o1;
                MetroMidiEvent mo2 = (MetroMidiEvent) o2;

                byte[] b1 = mo1.getMidiData();
                byte[] b2 = mo2.getMidiData();

                byte m1_channel = (byte) ( 0b01111 & b1[0] );
                byte m2_channel = (byte) ( 0b01111 & b2[0] );
                if ( m1_channel != m2_channel ) {
                    if ( m1_channel < m2_channel ) {
                        return -1;
                    } else {
                        return 1;
                    }
                }

                byte m1_status = (byte) ( 0b011110000 & b1[0] );
                byte m2_status = (byte) ( 0b011110000 & b2[0] );
                if ( m1_status != m2_status ) {
                    // 0b1000xxxx == Note Off
                    // 0b1001xxxx == Note On
                    if ( m1_status == 0b10010000 )
                        return -1;
                    else 
                        return 1;
                }
                
                return 0;
                        
            } else {
                return 0;
            }
        }
    };
    boolean isBetween(double from, double to);
    void setBarOffset(double barOffset);
    void calcBarOffset(int barLengthInFrames);
    double getBarOffset();
    
    /**
     * Check if the position of this event is inside the duration specified in the
     * parameter. See {@link MetroTrack#progressCursor(int, List) } for further
     * information.
     * 
     * This methods is called as a callback of JACKAudio processing; this method
     * should return as soon as possible. The heavy processing that blocks for
     * longer time than the current setting of JACK's frame rate causes JACK to
     * XRUN.
     * 
     * @param from
     *            Specifies the beginning point of the duration to check. The value
     *            is inclusive.
     * @param to
     *            Specifies the end point of the duration to check. The value is
     *            exclusive.
     * @return <code>true</code> if this event is inside the duration.
     */
    boolean isBetweenInFrames(int from, int to);
    void calcBarOffsetInFrames(int barLengthInFrames);
    int getBarOffsetInFrames();
    void setBarOffsetInFrames( int barOffsetInFrames );

    /*
     *  This method effectively converts MetroEvent into MetroMidiEvent.
     */
//    void      calcMidiOffset( int cursor );

    /**
     * Defines the procedure to execute when this event is activated. This method is
     * usually called when {@link #between(int, int)} returned <code>true</code>.
     * See {@link MetroTrack#progressCursor(int, List) } for further information.
     * 
     * This methods is called as a callback of JACKAudio processing; this method
     * should return as soon as possible. The heavy processing that blocks for
     * longer time than the current setting of JACK's frame rate causes JACK to
     * XRUN.
     * 
     * @param metro
     *            The Metro instance which is the owner of this event.
     * @param cursor TODO
     * @param from
     *            the value of <code>from</code> when {@link #between(int, int)}
     *            returns <code>true</code>.
     * @param to
     *            the value of <code>to</code> when {@link #between(int, int)}
     *            returns <code>true</code>.
     * @param nframes
     *            the current
     * @param eventList
     */
    MetroMidiEvent process(Metro metro, int cursor);
    
    
    /**
     * 
     * @param prefix
     * @return
     */
    public default String dump(String prefix) {
        StringBuilder sb = new StringBuilder();
        dumpProc(prefix, sb);
        return sb.toString();
    }

    void dumpProc(String prefix, StringBuilder sb);
}

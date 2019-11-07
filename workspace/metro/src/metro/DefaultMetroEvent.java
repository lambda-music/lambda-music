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
 * This class is the base class to manage all events that are sent/received between Metro framework and its client.
 * @author Ats Oka
 */
public abstract class DefaultMetroEvent implements MetroEvent {
    private String typeName;
    @Override
    public String getTypeName() {
        return typeName;
    }
    public DefaultMetroEvent( String typeName, double barOffset ) {
        super();
        this.typeName = typeName == null ? super.toString() : typeName;
        this.barOffset = barOffset;
    }
    public DefaultMetroEvent( String typeName, int barOffsetInFrames ) {
        super();
        this.typeName = typeName == null ? super.toString() : typeName;
        this.barOffsetInFrames = barOffsetInFrames;
    }

    private double barOffset;
    @Override
    public final double getBarOffset() {
        return barOffset;
    }
    @Override
    public void setBarOffset(double barOffset) {
        this.barOffset = barOffset;
    }
    @Override
    public final void prepareBarOffset( int barLengthInFrames ){
        this.barOffset = this.barOffsetInFrames / barLengthInFrames;
    }
    @Override
    public final boolean isBetween( double from, double to) {
        return from <= this.barOffset && this.barOffset < to;
    }
    
    private int barOffsetInFrames;
    @Override
    public final int getBarOffsetInFrames() {
        return barOffsetInFrames;
    }
    @Override
    public void setBarOffsetInFrames(int barOffsetInFrames) {
        this.barOffsetInFrames = barOffsetInFrames;
    }
    @Override
    public void prepareBarOffsetInFrames( int barLengthInFrames ){
        this.barOffsetInFrames = (int)(this.barOffset * barLengthInFrames );
    }
    @Override
    public boolean isBetweenInFrames( int from, int to ) {
        return from <= this.barOffsetInFrames && this.barOffsetInFrames < to;
    }
    
    @Override
    public void dumpProc( String prefix, StringBuilder sb ) {
        sb.append(prefix).append( "                id: " + getTypeName()          ).append( "\n" );
        sb.append(prefix).append( "            offset: " + getBarOffset()         ).append( "\n" );
        sb.append(prefix).append( "    offsetInFrames: " + getBarOffsetInFrames() ).append( "\n" );
    }
    
    @Override
    public String toString() {
        return "(MidiEvent " + getTypeName() + ")";
    }

//  public static void main(String[] args) {
//      MetroMidiEvent event = new MetroMidiEvent(1, 0.0d, new byte[] {} );
//      event.calcInFrames(48000);
//      boolean b = event.between(-1, 4);
//      System.out.println( b );
//  }

}

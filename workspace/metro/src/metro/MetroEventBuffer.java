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

import java.util.List;

import org.jaudiolibs.jnajack.JackException;

/**
 * This class represents all events which should be processed in a bar. 
 * 
 * @author Ats Oka
 */
public interface MetroEventBuffer extends MetroBufferedMidiReceiver<MetroMidiEvent>, MetroDumper {
    double getLength();
    void setLength(double length);
    int getLengthInFrames();
    int getBarLengthInFrames();
    double getActualLength();
    List<MetroEvent> getMetroEventList();
    int size();
    void prepare(int barLengthInFrames, boolean doSort) throws JackException;
    void exec(double offset, Runnable runnable);
    void event(MetroEvent event);
}





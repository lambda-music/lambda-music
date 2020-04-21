/*
 * Pulsar-Sequencer written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Pulsar-Sequencer. 
 * 
 * Pulsar-Sequencer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Pulsar-Sequencer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
 */

package lamu.lib.secretary;

import lamu.lib.evaluators.InvokableSchemeProcedure;

/**
 * This class defines an interface that executes an arbitrary invokable.
 * Currently there is only one class which implements this interface. See
 * {@link InvokableSchemeProcedure}
 * 
 * @author ats
 */
public abstract interface Invokable {
    Object NOARG = new Object[0];
    // not used
    Object NO_RESULT = new Object();
    /**
     * This method invokes the invokable which is denoted by the subclasses
     * implement this interface.
     * 
     * @param args
     * @return
     */
    abstract Object invoke( Object... args );
}

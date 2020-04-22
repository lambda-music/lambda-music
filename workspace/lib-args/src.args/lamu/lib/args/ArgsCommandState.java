package lamu.lib.args;

import java.util.Collection;

/**
 * A class to store the current state of the script engine.
 */
public class ArgsCommandState {
    public final Collection<ArgsCommand> availableCommands; 
    public ArgsCommandState( Collection<ArgsCommand> availableCommands ) {
        super();
        this.availableCommands = availableCommands;
    }
}

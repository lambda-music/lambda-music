package lamu;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.app.args.ArgsCommand;
import lamu.lib.app.args.ArgsCommandState;
import lamu.lib.stream.Stream;

/**
 * A class to store the current state of the script engine.
 */
public class LamuCommandState extends ArgsCommandState{
    public final Deque<ApplicationVessel> vessels;
    public final Deque<Stream> streamables;
    public LamuCommandState( Collection<ArgsCommand> availableCommands ) {
        super( availableCommands );
        this.vessels = new ArrayDeque<>();
        this.streamables = new ArrayDeque<>();
    }
}

package lamu.main;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;

import lamu.lib.apps.ApplicationVessel;
import lamu.lib.args.ArgsCommand;
import lamu.lib.args.ArgsCommandState;
import lamu.lib.streams.Stream;

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

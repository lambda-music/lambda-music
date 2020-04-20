package lamu.lib.app.args;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;

import lamu.lib.ConsoleChecker;
import lamu.lib.app.ApplicationVessel;
import lamu.lib.stream.NullStream;
import lamu.lib.stream.StdioStream;
import lamu.lib.stream.Stream;

/**
 * A class to store the current state of the script engine.
 */
public final class ArgsBuilderState {
    public Collection<ArgsCommand> availableCommands; 
    public Deque<ApplicationVessel> vessels;
    public Deque<Stream> streamables;
    public ArgsBuilderState( Collection<ArgsCommand> availableCommands ) {
        super();
        this.availableCommands = availableCommands;
        this.vessels = new ArrayDeque<>();
        this.streamables = new ArrayDeque<>();
        
        initStream();
    }

    /**
     * Set the default stream. In case it is executed by javaw (windows) STDIO is
     * not available and writing/reading from it causes a runtime exception to be
     * thrown which is not preferable. In order to avoid the exception, check
     * System.console(). When it returns null, it is likely that the current jvm is
     * executed from javaw.
     * 
     * See {@link lamu.LamuApplicationArgumentParser.StdioArgumentParserElementFactory}.
     * 
     * (Sun, 29 Mar 2020 03:35:24 +0900)
     */
    void initStream() {
        if ( ConsoleChecker.consoleExists() ) {
            this.streamables.push( StdioStream.INSTANCE );
        } else {
            this.streamables.push( NullStream.INSTANCE );
        }
    }
}
package lamu;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import quartz.lib.app.ApplicationComponent;

abstract class LamuCommand {
	abstract boolean match(List<String> arguments);
	abstract void    execute(List<LamuCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall);

	public static void parseSubargs( 
			List<LamuCommand> availableCommands, 
			List<ApplicationComponent> vessels, List<String> args, boolean recursiveCall )  
	{
		boolean done = false;
		for (LamuCommand c : availableCommands ) {
			if ( c.match( args ) ) {
				c.execute( availableCommands, vessels, args, recursiveCall );
				done = true;
				break;
			}
		}

		if (!done) {
			// This should not happen because default command always matches.
			throw new Error("unknown command");
		}
	}

	public static List<ApplicationComponent> parseArgs(
			List<LamuCommand> availableCommands, 
			String[] args ) throws IOException
	{
		List<List<String>> arrayOfSubargs = 
				LamuBeginEndSplitter.splitBeginEnd(Arrays.asList(args), "begin",  "end");

		List<ApplicationComponent> vessels = new ArrayList<>();
		for (Iterator<List<String>> i = arrayOfSubargs.iterator(); i.hasNext();) {
			List<String> subargs = i.next();
			parseSubargs(availableCommands, vessels, subargs, false);
		}
		return vessels;
	}
}
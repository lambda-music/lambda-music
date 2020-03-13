package lamu;

import java.util.List;

import lamu.lib.app.ApplicationComponent;

class LamuCommandExec extends LamuCommand {
	@Override
	boolean match(List<String> arguments) {
		return !arguments.isEmpty() && arguments.get(0).equals("exec");
	}

	@Override
	void execute(List<LamuCommand> availableCommands, List<ApplicationComponent> vessels, List<String> arguments, boolean recursiveCall) {
		List<String> subArguments = arguments.subList(1, arguments.size());
		// exec
		LamuApplicationArgumentParser argumentParser = new LamuApplicationArgumentParser();
		argumentParser.parse(subArguments);
		vessels.addAll(argumentParser.getApplicationVesselList());
	}
}
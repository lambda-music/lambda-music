package metro;

import java.util.List;

public interface MetroPortSelector {
	void selectPort( List<MetroPort> inputPorts, List<MetroPort> outputPorts, List<MetroPort> selectedInputPorts, List<MetroPort> selectedOutputPorts );

	static void executeSelector(
			Metro metro, 
			List<MetroPortSelector> portSelectors, 
			List<MetroPort> selectedInputPorts,
			List<MetroPort> selectedOutputPorts) 
	{
		List<MetroPort> inputPorts = metro.getInputPorts();
		List<MetroPort> outputPorts = metro.getOutputPorts();
		for ( MetroPortSelector selector : portSelectors ) {
			selector.selectPort(inputPorts, outputPorts, selectedInputPorts, selectedOutputPorts);
		}
	}
}

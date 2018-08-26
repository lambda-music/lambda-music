package ats.pulsar.old;

import java.util.ArrayList;
import java.util.List;

public class SamplePulsableBuilder implements PulsableBuilder {
	String name;
	@Override
	public String getName() {
		return "default";
	}
	
	@Override
	public List<Pulsable> create() {
		ArrayList<Pulsable> result = new ArrayList<Pulsable>();
		
		result = new ArrayList<Pulsable>();
		{
			JavaPulse[][] arr = {
					{ new JavaPulse(73) },
					{},
					{},
					{ new JavaPulse(73) },
					{},
					{ new JavaPulse(true, 73,90) },
			};
			result.add( new JavaPulseList( arr, 7,2, 3 ) );
		}
		{
			JavaPulse[][] arr = {
				{ new JavaPulse(63,80) },
			};
			result.add( new JavaPulseList( arr, 4 ,2, 0 ) );
		}
		{
			JavaPulse[][] arr = {
				{ new JavaPulse(57,80) },
			};
			result.add( new JavaPulseList( arr, 1 ,2, 0 ) );
		}
		
		return result;
	}
}
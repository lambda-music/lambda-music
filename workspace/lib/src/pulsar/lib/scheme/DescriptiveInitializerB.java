package pulsar.lib.scheme;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class DescriptiveInitializerB {
	public static final DescriptiveInitializerB EMPTY = new DescriptiveInitializerB( Arrays.asList() );
	List<Object> arguments;
	public DescriptiveInitializerB( Object ... arguments ) {
		this.arguments = Arrays.asList(arguments);
	}
	public DescriptiveInitializerB( List<Object> arguments ) {
		this.arguments = Collections.unmodifiableList( arguments );
	}
}

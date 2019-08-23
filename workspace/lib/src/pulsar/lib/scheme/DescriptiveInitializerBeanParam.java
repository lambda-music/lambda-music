package pulsar.lib.scheme;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class DescriptiveInitializerBeanParam {
	public static final DescriptiveInitializerBeanParam EMPTY = new DescriptiveInitializerBeanParam( Arrays.asList() );
	List<Object> arguments;
	public DescriptiveInitializerBeanParam( Object ... arguments ) {
		this.arguments = Arrays.asList(arguments);
	}
	public DescriptiveInitializerBeanParam( List<Object> arguments ) {
		this.arguments = Collections.unmodifiableList( arguments );
	}
}


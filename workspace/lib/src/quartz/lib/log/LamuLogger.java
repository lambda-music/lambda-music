package quartz.lib.log;

import java.util.logging.Logger;

public class LamuLogger {
	public static Logger getLogger( String name ) {
		return Logger.getLogger( name );
		//        return new SimpleConsoleLogger( name, null, Level.WARNING );
	}

}

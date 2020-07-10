package lamu;

import java.lang.invoke.MethodHandles;
import java.util.Objects;
import java.util.logging.Level;

import gnu.expr.SourceName;
import gnu.kawa.io.OutPort;
import gnu.mapping.Procedure0;
import gnu.mapping.Procedure2;
import gnu.mapping.Values;
import kawapad.Kawapad;
import kawapad.logging.KawapadHandler;
import kawapad.logging.LoggerOutPort;
import lamu.lib.logging.Logger;

public class logger {
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static final OutPort SEVERE = new OutPort( new LoggerOutPort(LOGGER, Level.SEVERE ),true, true );
	static final OutPort WARN = new OutPort( new LoggerOutPort(LOGGER, Level.WARNING ),true, true );
	static final OutPort INFO = new OutPort( new LoggerOutPort(LOGGER, Level.INFO    ),true, true );
	
	@SourceName(name = "severe-logger" )
	public static final Procedure0 severeLogger = new Procedure0() {
		@Override
		public Object apply0() throws Throwable {
			return SEVERE;
		}
	};
	@SourceName(name = "warn-logger" )
	public static final Procedure0 waringLogger = new Procedure0() {
		@Override
		public Object apply0() throws Throwable {
			return WARN;
		}
	};
	
	@SourceName(name = "info-logger" )
	public static final Procedure0 infoLogger = new Procedure0() {
		@Override
		public Object apply0() throws Throwable {
			return INFO;
		}
	};

	@SourceName(name = "set-logging-level" )
	public static final Procedure2 setLogginLevel = new Procedure2() {
		@Override
		public Object apply2( Object arg0, Object arg1 ) throws Throwable {
			Kawapad kawapad = (Kawapad) arg0;
			String level = Objects.toString(arg1).toUpperCase();
			KawapadHandler.setLoggingLevel(kawapad, level);
			return Values.empty;
		}

	};
	
	
}

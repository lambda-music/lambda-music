package kawapad.logging;

import java.io.IOException;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;

public class LoggerOutPort extends Writer {
	private final StringBuilder buf = new StringBuilder();
	private final Logger logger;
	private final Level level;
	public LoggerOutPort(Logger logger, Level level ) {
		super();
		this.logger = logger;
		this.level = level;
	}

	@Override
	public synchronized void write(char[] cbuf, int off, int len) throws IOException {
		buf.append(cbuf,off,len);
	}

	@Override
	public synchronized void flush() throws IOException {
		logger.log(level, buf.toString());
		buf.setLength(0);
	}

	@Override
	public void close() throws IOException {
		this.flush();
	}
}

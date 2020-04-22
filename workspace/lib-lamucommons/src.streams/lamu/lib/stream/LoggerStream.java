package lamu.lib.stream;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;


public class LoggerStream implements Stream {
    Stream streamable;
    InputStream inputStream;
    OutputStream outputStream;
    InputStream errorStream;
    OutputStream inputStreamLog;
    OutputStream outputStreamLog;
    OutputStream errorStreamLog;
    public LoggerStream( Stream streamable, File inputStreamLogFile, File outputStreamLogFile, File errorStreamLogFile) throws FileNotFoundException {
        this(streamable, 
            new FileOutputStream( inputStreamLogFile ), 
            new FileOutputStream( outputStreamLogFile ), 
            new FileOutputStream( errorStreamLogFile ));
    }

    public LoggerStream( Stream streamable, OutputStream inputStreamLog, OutputStream outputStreamLog, OutputStream errorStreamLog ) throws FileNotFoundException {
        super();
        this.streamable      = streamable;
        this.inputStreamLog  = inputStreamLog;
        this.outputStreamLog = outputStreamLog;
        this.errorStreamLog  = errorStreamLog;
        this.inputStream     = new LoggingInputStream() {
            @Override
            public InputStream in() {
                return streamable.getDownwardStream();
            }
            @Override
            public OutputStream log() {
                return inputStreamLog;
            }
        };
        this.outputStream = new LoggingOutputStream() {
            @Override
            public OutputStream out() {
                return streamable.getUpwardStream();
            }
            @Override
            public OutputStream log() {
                return outputStreamLog;
            }
        };
        this.errorStream = new LoggingInputStream() {
            @Override
            public InputStream in() {
                return streamable.getDownwardErrorStream();
            }
            @Override
            public OutputStream log() {
                return errorStreamLog;
            }
        };
    }

    @Override
    public InputStream getDownwardStream() {
        return inputStream;
    }

    @Override
    public InputStream getDownwardErrorStream() {
        return errorStream;
    }

    @Override
    public OutputStream getUpwardStream() {
        return outputStream;
    }
    @Override
    public String toString() {
        return streamable.toString() + " w/logger";
    }
}

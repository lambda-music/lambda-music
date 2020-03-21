package lamu.lib.stream;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;


public class LoggingStreamable implements Streamable {
    Streamable streamable;
    InputStream inputStream;
    OutputStream outputStream;
    InputStream errorStream;
    OutputStream inputStreamLog;
    OutputStream outputStreamLog;
    OutputStream errorStreamLog;
    public LoggingStreamable( Streamable streamable, File inputStreamLogFile, File outputStreamLogFile, File errorStreamLogFile) throws FileNotFoundException {
        this(streamable, 
            new FileOutputStream( inputStreamLogFile ), 
            new FileOutputStream( outputStreamLogFile ), 
            new FileOutputStream( errorStreamLogFile ));
    }

    public LoggingStreamable( Streamable streamable, OutputStream inputStreamLog, OutputStream outputStreamLog, OutputStream errorStreamLog ) throws FileNotFoundException {
        super();
        this.streamable = streamable;
        this.inputStreamLog = inputStreamLog;
        this.outputStreamLog = outputStreamLog;
        this.errorStreamLog = errorStreamLog;
        this.inputStream = new LoggingInputStream() {
            @Override
            public InputStream in() {
                return streamable.getInputStream();
            }
            @Override
            public OutputStream log() {
                return inputStreamLog;
            }
        };
        this.outputStream = new LoggingOutputStream() {
            @Override
            public OutputStream out() {
                return streamable.getOutputStream();
            }
            @Override
            public OutputStream log() {
                return outputStreamLog;
            }
        };
        this.errorStream = new LoggingInputStream() {
            @Override
            public InputStream in() {
                return streamable.getErrorStream();
            }
            @Override
            public OutputStream log() {
                return errorStreamLog;
            }
        };
    }

    @Override
    public InputStream getInputStream() {
        return inputStream;
    }

    @Override
    public InputStream getErrorStream() {
        return errorStream;
    }

    @Override
    public OutputStream getOutputStream() {
        return outputStream;
    }

}

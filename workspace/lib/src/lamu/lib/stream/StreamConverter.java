package lamu.lib.stream;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

public class StreamConverter implements ServersideStream {
    public static ServersideStream reverse( ClientsideStream from ) {
        return new StreamConverter( from );
    }
    private ClientsideStream clientside;
    public StreamConverter( ClientsideStream clientside ) {
        this.clientside = clientside;
    }

    OutputStream upwardStream = new LatebindOutputStream() {
        OutputStream out=null;
        @Override
        public synchronized OutputStream out() {
            if ( out == null ) {
                out = new PipedOutputStream( new PipedInputStream() );
                out.
            }
            return out;
        }
    };
    InputStream  downwardStream = new LatebindInputStream() {
        @Override
        public InputStream in() {
            return clientside.getUpwardStream();
        }
    };
    InputStream  downwardErrorStream;
    @Override
    public InputStream getDownwardErrorStream() {
        return downwardErrorStream;
    }
    @Override
    public InputStream getDownwardStream() {
        return downwardStream;
    }
    @Override
    public OutputStream getUpwardStream() {
        return upwardStream;
    }
    

}

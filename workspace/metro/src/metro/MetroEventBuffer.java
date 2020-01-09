package metro;

import static metro.Metro.*;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import org.jaudiolibs.jnajack.JackException;

import pulsar.lib.log.PulsarLogger;

class MetroEventBuffer extends MetroBufferedToNonBufferedMidiReceiver<MetroEvent,byte[]> implements MetroDumper {
    public static MetroEventBuffer create() {
        return new MetroEventBuffer();
    }
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    MetroEventBuffer() {
        super( MetroMidiMessage.getInstance() );
    }
    
    private boolean endCalled=false;
    @Override
    public boolean endCalled() {
        return endCalled;
    }
    @Override
    public MetroEvent end() {
        this.endCalled = true;
        return null;
    }
    
    private double length = 1.0d;
    private boolean prepared = false;
    private int barLengthInFrames=-1;
    private int lengthInFrames = -1;
    private final List<MetroEvent> metroEventList = new ArrayList<MetroEvent>(10);
    
    double getLength() {
        return length;
    }
    @Override
    public MetroEvent error(double offset, MetroPort port, String message) {
        logWarn( message );
        return null;
    }
    @Override
    public MetroEvent length(double length) {
        if (DEBUG) 
            logInfo( "setLength():" + length );
        this.length = length;
        return null;
    }
    
    //  public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity ) {
    //      noteHit( offset, outputPort, channel, note, velocity, -1 );
    //  }
    //  public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity, double duration ) {
    //      if ( duration < 0 )
    //          duration = 0.0025d;
    //      
    //      noteOn(  offset,            outputPort, channel, note, velocity );
    //      noteOff( offset + duration, outputPort, channel, note, velocity );
    //  }
    
    @Override
    public final MetroEvent event( double offset, MetroEvent event ) {
        // Add it to the list.
        return receive(event);
    }
    
    @Override
    public MetroEvent exec( double offset, Runnable runnable ) {
        //      super.exec( offset, runnable );
        return receive( new MetroMessageEvent( "exec", offset, runnable ) ); 
    }
    
    
    public double getActualLength() {
        double max = 0;
        for ( MetroEvent e : this.metroEventList )
            if ( max < e.getBarOffset() ) 
                max = e.getBarOffset();
        
        return max;
    }
    public int getBarLengthInFrames() {
        if ( ! prepared )
            throw new RuntimeException("not prepared");
        return barLengthInFrames;
    }
    public int getLengthInFrames() {
        if ( ! prepared )
            throw new RuntimeException("not prepared");
        return lengthInFrames;
    }
    
    public List<MetroEvent> getMetroEventList() {
        return metroEventList;
    }
    
    public void prepare( int barLengthInFrames, boolean doSort ) throws JackException {
        if ( doSort && false )
            this.metroEventList.sort( MetroEvent.BAR_OFFSET_COMPARATOR );
        
        //        int barLengthInFrames = Metro.calcBarInFrames( metro, client, position );
        this.prepareBarOffsetInFrames( barLengthInFrames );
        
        this.prepared = true;
    }
    
    private void prepareBarOffsetInFrames( int barLengthInFrames ) {
        //      System.out.println("MetroMidiEventBuffer.calcInFrames() barInFrames="  + barInFrames );
        for ( MetroEvent e : this.metroEventList ) {
            e.prepareBarOffsetInFrames( barLengthInFrames );
        }
        //      System.out.println( "this.length " + this.length  );
        this.barLengthInFrames = barLengthInFrames;
        this.lengthInFrames = (int) (this.length * (double)barLengthInFrames);
        
        if ( DEBUG ) 
            logInfo( "MetroMidiEventBuffer.calcInFrames() barInFrames="  + barLengthInFrames + " / lengthInFrames=" + this.lengthInFrames  + "/ length=" + this.length);
    }
    
    public int size() {
        return this.metroEventList.size();
    }
    
    MetroEvent receive(MetroEvent event) {
        this.metroEventList.add(event);
        return event;
    }
    
    @Override
    public final MetroEvent receive( String id, double offset, MetroPort outputPort, byte[] data ) {
        if ( data == null )
            return null;
        
        //        logInfo( "midiEvent:" + SchemeUtils.bytesToString( data ) );
        // Create an event object.
        
        // Add it to the list.
        return receive( new DefaultMetroEventMidiEvent( id, offset, outputPort, data ) );
    }
    
    @Override
    public void dumpProc(String prefix, StringBuilder sb) {
        sb.append( prefix ).append( "length         : " + this.length        ).append( "\n" );
        sb.append( prefix ).append( "lengthInFrames : " + this.lengthInFrames).append( "\n" );
        int i = 0;
        for ( MetroEvent e : this.metroEventList ) {
            sb.append( prefix ).append( "    No" + i).append( "\n" );
            sb.append( prefix ).append( e.dump( "    " )).append( "\n" );
            i++;
        }
        sb.append( prefix ).append( "    END");
    }
}

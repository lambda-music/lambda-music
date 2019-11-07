package metro;

import static metro.Metro.*;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.JackException;

public class DefaultMetroEventBuffer extends MetroBufferedToNonBufferedMidiReceiver<MetroMidiEvent,byte[]> implements MetroEventBuffer {
    public static MetroEventBuffer create() {
        return new DefaultMetroEventBuffer();
    }
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    DefaultMetroEventBuffer() {
        super( MetroMidiMessage.getInstance() );
    }
    
    private double length = 1.0d;
    private boolean prepared = false;
    private int barLengthInFrames=-1;
    private int lengthInFrames = -1;
    private final List<MetroEvent> metroEventList = new ArrayList<MetroEvent>(10);
    
    @Override
    public double getLength() {
        return length;
    }
    @Override
    public void setLength(double length) {
        if (DEBUG) 
            logInfo( "setLength():" + length );
        this.length = length;
    }
    @Override
    public double getActualLength() {
        double max = 0;
        for ( MetroEvent e : this.metroEventList )
            if ( max < e.getBarOffset() ) 
                max = e.getBarOffset();
        
        return max;
    }
    @Override
    public int getBarLengthInFrames() {
        if ( ! prepared )
            throw new RuntimeException("not prepared");
        return barLengthInFrames;
    }
    @Override
    public int getLengthInFrames() {
        if ( ! prepared )
            throw new RuntimeException("not prepared");
        return lengthInFrames;
    }
    
    @Override
    public List<MetroEvent> getMetroEventList() {
        return metroEventList;
    }
    
    @Override
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
    
    @Override
    public int size() {
        return this.metroEventList.size();
    }

    @Override
    public final void event( MetroEvent event ) {
        // Add it to the list.
        this.metroEventList.add(event);
    }
    
    @Override
    public final MetroMidiEvent convertResult( String id, double offset, MetroPort outputPort, byte[] data ) {
//        logInfo( "midiEvent:" + SchemeUtils.bytesToString( data ) );
        // Create an event object.
        DefaultMetroEventMidiEvent event = new DefaultMetroEventMidiEvent( id, offset, outputPort, data );
        
        // Add it to the list.
        this.metroEventList.add(event);
        
        return event;
    }
    
    public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity ) {
        noteHit( offset, outputPort, channel, note, velocity, -1 );
    }
    public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity, double duration ) {
        if ( duration < 0 )
            duration = 0.0025d;
        
        noteOn(  offset,            outputPort, channel, note, velocity );
        noteOff( offset + duration, outputPort, channel, note, velocity );
    }

    @Override
    public void exec( double offset, Runnable runnable ) {
        MetroMessageEvent event = new MetroMessageEvent( "exec", offset, runnable );

        this.metroEventList.add( event );
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

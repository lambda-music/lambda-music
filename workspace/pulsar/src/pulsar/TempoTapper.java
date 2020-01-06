package pulsar;

import java.util.ArrayList;
import java.util.List;

import pulsar.Pulsar.TempoTapperTempoNotifier;

class TempoTapper {
    final List<TempoTapperTempoNotifier> notifiers = new ArrayList<>();
    void registerNotifier( TempoTapperTempoNotifier notifier ) {
        notifiers.add( notifier );
    }
    
    long prev_time = 0;
    int BUF_SIZE = 3;
    long TIMEOUT = 1000L*1000L*1000L*2L;
    void reset() {
        for ( int i=0;i<t.length; i++ )
            t[i]=0;
        tidx=0;
    }
    long t[] = new long[BUF_SIZE];
    int tidx= 0;
    
    public double tap( double currentBeatsPerMinute ) {
        long current_time = System.nanoTime();
        if ( prev_time == 0 ) {
            prev_time = current_time;
            return -1;
        }
        if ( TIMEOUT < current_time - prev_time ) {
            prev_time = current_time;
            reset();
            return -1;
        }

        long current_diff = current_time - prev_time ;
        Pulsar.logInfo( "Elapsed Time : " + current_diff );

        tidx ++;
        if( tidx < t.length ) {
        } else {
            tidx = 0;
        }

        t[tidx] = current_diff;
        prev_time = current_time;

        boolean isFull = true;
        long sum = 0;
        {
            for ( int i=0; i<t.length; i++ ) {
                if ( 0 < t[i] ) {
                    sum += t[i];
                } else {
                    isFull = false;
                    break;
                }
            }
        }

        if ( isFull ) { 
            double avg = (double)sum / t.length;
            double onemin = 1000L*1000L*1000L*60L;
            double beatsPerMinute =  onemin / avg  ;
            
            beatsPerMinute = ( beatsPerMinute + currentBeatsPerMinute * 2 ) / 3;
            Pulsar.logInfo( String.format( "%.2f / %.2f = %.2f", onemin , avg , beatsPerMinute  ) );
            
            return beatsPerMinute;
        }
        
        return -1;
    }
}
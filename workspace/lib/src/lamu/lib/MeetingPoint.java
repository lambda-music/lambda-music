package lamu.lib;

import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

import lamu.utils.lib.MersenneTwisterFast;

public class MeetingPoint {
    private static final MeetingPoint INSTANCE = new MeetingPoint();
    public static MeetingPoint getInstance() {
        return INSTANCE;
    }
    private MeetingPoint() {
    }
    private final MersenneTwisterFast mt = new MersenneTwisterFast();
    private final HashMap<String,Object> map = new HashMap<>();
    private final Timer timer = new Timer( "MeetingPointWatchDog", true);
    public synchronized String addValue( Object value ) {
        String key = Long.toHexString( mt.nextLong() ) + Long.toHexString( mt.nextLong() ); 
        map.put(key, value);
        timer.schedule( new TimerTask() {
            @Override
            public void run() {
                removeValue(key);
            }
        }, 1000*60 );
        return key;
    }
    public synchronized void removeValue(String key) {
        map.remove(key);
    }
    public synchronized Object getValue( String key ) {
        return map.get(key);
    }
    public Timer getTimer() {
        return timer;
    }
}

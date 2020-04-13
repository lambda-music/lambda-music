package lamu.utils.lib;

import lamu.lib.secretary.Invokable;

public class PulsarSharedTimer {
    public static final java.util.Timer timer = new java.util.Timer("PulsarTimer", true );
    static {
//        addCleanupHook( new Runnable() {
//            @Override
//            public void run() {
//                timer.cancel();
//            }
//        });
    }
    public static Runnable createTimer( Runnable threadInitializer, long delay, long interval, Invokable invokable ) {
        synchronized ( timer ) {
            if ( 0 < interval  ){
                timer.scheduleAtFixedRate( new java.util.TimerTask() {
                    @Override
                    public void run() {
                        if ( threadInitializer != null ) 
                            threadInitializer.run();
                        
                        // Execute the specified process.
                        Object result = invokable.invoke();
                        
                        if ( Boolean.FALSE.equals( result ) ) {
                            timer.cancel();
                        }
                    }
                }, delay, interval );
            } else {
                timer.schedule( new java.util.TimerTask() {
                    @Override
                    public void run() {
                        if ( threadInitializer != null ) 
                            threadInitializer.run();
                        
                        // Execute the specified process.
                        invokable.invoke();
                    }
                }, delay );
            }
            
            return new Runnable() {
                @Override
                public void run() {
                    synchronized ( timer ) {
                        timer.cancel();
                    }
                }
            };
        }
    }
}

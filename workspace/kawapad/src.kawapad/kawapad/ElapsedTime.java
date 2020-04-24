package kawapad;

public class ElapsedTime {
    long stime=-1;
    long etime=-1;
    public void start() {
        this.stime = System.nanoTime();
    }
    public void end() {
        this.etime = System.nanoTime();
    }
    public String getMessage( String name ) { 
        return format( name, elapsedTime() );
    }
    public static String format(String name, double t) {
        return String.format( "%-30s: t=% 6.1f", name, t );
    }
    public double elapsedTime() {
        return ((double)elapsedNanoTime())/1000/1000;
    }
    public long elapsedNanoTime() {
        return etime - stime;
    }
}

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
        return String.format( "%-30s: t=% 6.1f", name, (((double)(etime-stime))/1000/1000) );
    }
}

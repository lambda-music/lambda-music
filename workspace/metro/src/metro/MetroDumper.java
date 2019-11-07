package metro;

public interface MetroDumper {
    /**
     * 
     * @param prefix
     * @return
     */
    public default String dump(String prefix) {
        StringBuilder sb = new StringBuilder();
        dumpProc(prefix, sb);
        return sb.toString();
    }
    
    void dumpProc(String prefix, StringBuilder sb);
}

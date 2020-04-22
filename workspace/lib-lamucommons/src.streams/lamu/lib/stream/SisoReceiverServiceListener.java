package lamu.lib.stream;

public interface SisoReceiverServiceListener extends SisoReceiverListener {
    /**
     * The parent of this listener will call this method once in order to acknowledge the
     * reference of the parent when this listener is passed to its constructor. 
     * 
     * @param receiver
     *     a reference to the parent receiver.  
     */
    public void notifyParent( SisoReceiver receiver );
    public void start(SisoReceiver receiver);
    public void end(SisoReceiver receiver);
}

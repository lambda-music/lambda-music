package metro;

import java.util.List;

public interface MetroEventProcess {

    /*
     *  This method effectively converts MetroEvent into MetroMidiEvent.
     */
//    void      calcMidiOffset( int cursor );

    /**
     * Defines the procedure to execute when this event is activated. This method is
     * usually called when {@link #between(int, int)} returned <code>true</code>.
     * See {@link MetroTrack#progressCursor(Metro, MetroTrack, long, long, List, List, List, List, List) } for further information.
     * 
     * This methods is called as a callback of JACKAudio processing; this method
     * should return as soon as possible. The heavy processing that blocks for
     * longer time than the current setting of JACK's frame rate causes JACK to
     * XRUN.
     * 
     * @param metro
     *            The Metro instance which is the owner of this event.
     * @param cursor TODO
     * @param from
     *            the value of <code>from</code> when {@link #between(int, int)}
     *            returns <code>true</code>.
     * @param to
     *            the value of <code>to</code> when {@link #between(int, int)}
     *            returns <code>true</code>.
     * @param nframes
     *            the current
     * @param eventList
     */
    void process(Metro metro, long cursor);
}

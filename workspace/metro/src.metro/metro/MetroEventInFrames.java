package metro;

import java.util.List;

public interface MetroEventInFrames extends MetroEventProcess {
    /**
     * Check if the position of this event is inside the duration specified in the
     * parameter. See {@link MetroTrack#progressCursor(Metro, MetroTrack, long, long, List, List, List, List, List) } for further
     * information.
     * 
     * This methods is called as a callback of JACKAudio processing; this method
     * should return as soon as possible. The heavy processing that blocks for
     * longer time than the current setting of JACK's frame rate causes JACK to
     * XRUN.
     * 
     * @param from
     *            Specifies the beginning point of the duration to check. The value
     *            is inclusive.
     * @param to
     *            Specifies the end point of the duration to check. The value is
     *            exclusive.
     * @return <code>true</code> if this event is inside the duration.
     */
    boolean isBetweenInFrames(long from, long to);
    void prepareBarOffsetInFrames(long barLengthInFrames);
    long getBarOffsetInFrames();
    void setBarOffsetInFrames( long barOffsetInFrames );
}


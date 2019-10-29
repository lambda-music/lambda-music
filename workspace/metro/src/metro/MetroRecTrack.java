package metro;

public class MetroRecTrack implements MetroSyncTrack {
    int cursor = -1;
    MetroSyncTrack syncTrack;
    int syncOffset;
    public MetroRecTrack(MetroSyncTrack syncTrack, int syncOffset ) {
        super();
        this.syncTrack = syncTrack;
        this.syncOffset = syncOffset;
    }

    @Override
    public int getCursor() {
        return this.cursor;
    }

    @Override
    public void setCursor(int cursor) {
        this.cursor = cursor;
    }

    @Override
    public MetroSyncType getSyncType() {
        return MetroSyncType.IMMEDIATE;
    }

    @Override
    public MetroSyncTrack getSyncTrack() {
        return this.syncTrack;
    }

    @Override
    public double getSyncOffset() {
        return this.syncOffset;
    }

    @Override
    public int getLatestLengthInFrames() {
        return -1;
    }
    
}

package metro;

public interface MetroSyncSequence extends MetroSequence {
    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly getCursor() 
    public abstract long getCurrentPositionInFrames(Metro metro);
    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly setCursor() 
    public abstract void setCurrentPositionInFrames(Metro metro, long positionInFrames);
    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly getLatestLengthInFrames() 
    public abstract long getCurrentLengthInFrames(Metro metro);
    // (Tue, 05 May 2020 18:58:13 +0900) This method was formerly getPosition() 
    public abstract double getPosition(Metro metro);

}

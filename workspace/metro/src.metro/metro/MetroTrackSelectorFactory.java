package metro;

public interface MetroTrackSelectorFactory {
    public abstract MetroTrackSelector createSelector( Object[] args );
}

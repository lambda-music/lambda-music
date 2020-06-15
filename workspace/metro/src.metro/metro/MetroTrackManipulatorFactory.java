package metro;

public interface MetroTrackManipulatorFactory {
    MetroTrackManipulator create( Object... args );
    
//    default MetroTrackManipulator create( List<MetroTrackSelector> trackSelectors, MetroTrackSynchronizer trackSynchronizer ) {
//        MetroTrackManipulator[] result = new MetroTrackManipulator[ trackSelectors.size() ];
//        int i=0;
//        for ( MetroTrackSelector trackSelector : trackSelectors ) {
//            result[i++] = this.create( trackSelector , trackSynchronizer);
//        }
//        return MetroTrackManipulatorBasic.multiple( result );
//    }
}

package lamu.lib.thread;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ThreadInitializerUtils {
    /*
     * Initialize ThreadInitializer
     *   vessel.getThreadInitializerCollection().addAllThreadInitializer( 
     *       ThreadInitializerUtils.collectThreadInitializers( allObjects ));
     */
    public static List<ThreadInitializer> collectThreadInitializers(Collection<?> list) {
        // Collect thread initializers and set to collections.
        ArrayList<ThreadInitializer> threadInitializerList = new ArrayList<>(); 
        ArrayList<ThreadInitializerCollection> threadInitializerCollectionList = new ArrayList<>(); 
        {
            // Collect all thread initializers.
            ThreadInitializerUtils.addInitializerContainer( threadInitializerList, list );

            // Collect all thread initializer collections.
            ThreadInitializerUtils.addInitializerCollectionContainer( threadInitializerCollectionList, list );
            
            // then, add the initializers to the collections.
            for ( ThreadInitializerCollection c : threadInitializerCollectionList ) {
                ThreadInitializerUtils.addAllThreadInitializers( c, threadInitializerList );
            }
        }
        return threadInitializerList;
    }

    public static void addInitializerContainer( Collection<ThreadInitializer> destination, Collection<?> source ) {
        for ( Object o : source ) {
            if ( o instanceof ThreadInitializerContainer ) {
                ThreadInitializer threadInitializer = ((ThreadInitializerContainer)o).getThreadInitializer();
                if ( threadInitializer.isPublished() ) {
                    destination.add(threadInitializer);
                    // Add the only first one.  (Fri, 20 Dec 2019 05:01:24 +0900)
                    break;
                }
            }
        }
    }
    public static void addInitializerCollectionContainer( Collection<ThreadInitializerCollection> destination, Collection<?> source ) {
        for ( Object o : source ) {
            if ( o instanceof ThreadInitializerCollectionContainer ) {
                destination.add(((ThreadInitializerCollectionContainer)o).getThreadInitializerCollection());
            }
        }
    }
    public static void addAllThreadInitializers(
        ThreadInitializerCollection initializerCollection,
        ArrayList<ThreadInitializer> threadInitializers) 
    {
        initializerCollection.addAllThreadInitializer( threadInitializers );
    }
}

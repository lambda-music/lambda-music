package lamu.lib.app;

import lamu.lib.CurrentObject;
import lamu.lib.thread.ThreadInitializer;
import lamu.lib.thread.ThreadInitializerContainer;

public class StaticApplicationVessel extends ApplicationVessel implements ThreadInitializerContainer<StaticApplicationVessel> {
    public StaticApplicationVessel(String name) {
        super(name);
    }
    private static final CurrentObject<StaticApplicationVessel> currentObject = new CurrentObject<>( StaticApplicationVessel.class );
    private final ThreadInitializer<StaticApplicationVessel> threadInitializer =
            ThreadInitializer.createMultipleThreadInitializer( "application-vessel", this, 
                ThreadInitializer.createThreadInitializer( "application-vessel-current", currentObject, this ) );
            
    @Override
    public ThreadInitializer<StaticApplicationVessel> getThreadInitializer() {
        return threadInitializer;
    }
    public static StaticApplicationVessel getCurrent() {
        return currentObject.get();
    }
    public static boolean isPresent() {
        return currentObject.isPresent();
    }
}

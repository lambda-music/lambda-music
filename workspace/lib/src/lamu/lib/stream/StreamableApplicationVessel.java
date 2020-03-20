package lamu.lib.stream;

import lamu.lib.app.ApplicationVessel;

public class StreamableApplicationVessel {

    public static StandardStream vessel2stream( ApplicationVessel vessel ) {
        return (StandardStream)vessel.getComponents().get(0);
    }

}

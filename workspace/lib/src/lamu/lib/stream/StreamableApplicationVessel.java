package lamu.lib.stream;

import lamu.lib.app.ApplicationVessel;

public class StreamableApplicationVessel {

    public static Streamable vessel2stream( ApplicationVessel vessel ) {
        return (Streamable)vessel.getComponents().get(0);
    }

}

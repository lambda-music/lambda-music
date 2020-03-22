package lamu.lib.stream;

import lamu.lib.app.ApplicationVessel;

public class StreamableApplicationVessel {

    public static ServersideStream vessel2stream( ApplicationVessel vessel ) {
        return (ServersideStream)vessel.getComponents().get(0);
    }

}

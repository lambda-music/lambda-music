package lamu;

import java.io.InputStream;
import java.io.OutputStream;

import lamu.lib.app.ApplicationVessel;
import lamu.lib.stream.Streamable;

class ForkedApplicationVessel extends ApplicationVessel implements Streamable {
    ForkedApplicationVessel(String name) {
        super(name);
    }
    @Override
    public InputStream getInputStream() {
        return
            lamu.lib.stream.StreamableApplicationVessel.vessel2stream(this).getInputStream();
    }
    @Override
    public InputStream getErrorStream() {
        return
            lamu.lib.stream.StreamableApplicationVessel.vessel2stream(this).getErrorStream();
    }
    @Override
    public OutputStream getOutputStream() {
        return
            lamu.lib.stream.StreamableApplicationVessel.vessel2stream(this).getOutputStream();
    }
}
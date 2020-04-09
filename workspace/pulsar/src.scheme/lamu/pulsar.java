package lamu;

import gnu.mapping.Environment;
import pulsar.PulsarDocuments;
import pulsar.PulsarLib;
import pulsar.PulsarNoteListParser;

public class pulsar implements Runnable {
    @Override
    public void run() {
        Environment env = Environment.getCurrent();
        PulsarLib.initScheme( env );
        PulsarDocuments.defineDoc( env , PulsarNoteListParser.getInstance() );
    }
}

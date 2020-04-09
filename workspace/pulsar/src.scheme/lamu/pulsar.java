package lamu;

import gnu.mapping.Environment;
import pulsar.PulsarLib_Notes;
import pulsar.PulsarLib_Procs;
import pulsar.PulsarNoteListParser;

public class pulsar implements Runnable {
    @Override
    public void run() {
        Environment env = Environment.getCurrent();
        PulsarLib_Procs.initScheme( env );
        PulsarLib_Notes.defineDoc( env , PulsarNoteListParser.getInstance() );
    }
}

package lamu.lib.scheme.repl;

public interface SisoListener {
    public void start(SisoReceiver receiver);
    public void end(SisoReceiver receiver);
}

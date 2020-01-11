package pulsar.lib.app.args;

public interface ArgumentParserElement {
    /*
     * The returned value becomes to the "current" element afterwards;
     * usually it should return "this".
     */
    abstract ArgumentParserElement notifyArg( ArgumentParser parser, String s );
    abstract void notifyEnd(ArgumentParser parser);
}
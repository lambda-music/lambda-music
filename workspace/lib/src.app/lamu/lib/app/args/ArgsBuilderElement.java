package lamu.lib.app.args;

public interface ArgsBuilderElement {
    /*
     * The returned value becomes to the "current" element afterwards;
     * usually it should return "this".
     */
    abstract ArgsBuilderElement notifyArg( ArgsBuilder parser, String s );
    abstract void notifyEnd(ArgsBuilder parser);
}
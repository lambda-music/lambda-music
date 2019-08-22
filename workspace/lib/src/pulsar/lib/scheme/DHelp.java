package pulsar.lib.scheme;

public class DHelp extends DProcedure0 {
	public DHelp(String name) {
		super(name);
	}
	@Override
	public Object apply0() throws Throwable {
		String name = this.getName();
		String s = String.format( 
			       "#| (%1$s) is a documentation procedure.\n "
				 + " | Execute this expression to see (help %1$s)\n"
				 + "  |# help %1$s \n",  name );
		return SchemeUtils.executeSchemePageWrapper( SchemeUtils.toSchemeString( s ));
	}
}

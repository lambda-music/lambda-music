package pulsar.lib.scheme.scretary;

import gnu.expr.Language;
import gnu.mapping.Environment;
import pulsar.lib.secretary.Invokable;

public class SpecialInvokable implements Invokable {
	Invokable invokable;
	Language language;
	Environment environment;
	public SpecialInvokable( Invokable invokable, Language language ,Environment environment ) {
		this.invokable = invokable;
		this.language = language;
		this.environment = environment;
	}
	@Override
	public Object invoke(Object... args) {
		Environment.setCurrent( environment );
		Language.setCurrentLanguage( language );
		return invoke( args );
	}
}

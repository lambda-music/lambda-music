package ats.pulsar.lib.kawa.lambda.legacy;

import gnu.expr.Language;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;

public final class SchemeFunctionExecutor {
	// now this diverted to Pulsar.createInvocable2() (Mon, 22 Jul 2019 14:47:49 +0900)
	public static SchemeFunctionExecutor createSchemeFunctionExecutor(Procedure procedure) {
		return new SchemeFunctionExecutor(procedure);
	}
	private final Environment env;
	private final Language lang;
	private final Procedure procedure;
	private SchemeFunctionExecutor(Procedure procedure ) {
		this.env = Environment.getCurrent();
		this.lang = Language.getDefaultLanguage();
		this.procedure = procedure;
	}
	public Object execute( Object ... args ) {
		try {
			Environment.setCurrent(env);
			Language.setCurrentLanguage(lang);
			return this.procedure.applyN( args );
		} catch (Throwable e1) {
			LegacyKawaUtils.logError( "" , e1 );
			return e1;
		}
	}
}
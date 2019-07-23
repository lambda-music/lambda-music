package ats.pulsar.lib.kawa.secretary;

import javax.swing.SwingUtilities;

import ats.pulsar.lib.secretary.Invokable;
import ats.pulsar.lib.secretary.SecretariallyInvokable;
import ats.pulsar.lib.secretary.Secretary;
import ats.pulsar.lib.secretary.SecretaryMessage;
import gnu.expr.Language;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;

public class SchemeSecretary extends Secretary<Scheme> {
	public SchemeSecretary() {
	}
	public Invokable createSecretarillyInvokable( Procedure procedure ) {
		return new SecretariallyInvokable( this, new InvokableSchemeProcedure( procedure ) );  
	}
	public Scheme getScheme() {
		return getExecutive();
	}
	
	/*-
	 * Memorandum 1 (Someday in 2018)
	 * 
	 * This field specifies an environment object which contains a scope of scheme
	 * language to be executed with.
	 * 
	 * I think the necessity of this field is needed to be explained.
	 * 
	 * Usually these environment objects are not supposed to be set manually. The
	 * reason why it to be set manually is that sometimes the environment object is
	 * missing. I am still not sure what exactly condition causes it to be gone, but
	 * it occurs sometimes.
	 * 
	 * It seemed to me that Kawa's environment objects are stored per a thread. I am
	 * still not sure when they are created. But when multiple threads access to a
	 * single Kawa scheme object, sometime the environment object on the scheme
	 * object is gone and this phenomenon causes strange errors.
	 * 
	 * A working around which I found is setting the desired environment object
	 * before calling the invokable.
	 * 
	 * This field keeps the environment object which is in charge when the invokable
	 * (closure) was passed in order to set it it every time before calling the
	 * invokable.
	 * 
	 * I am quite sure this is not proper way to solve the problem, but it worked
	 * for me.
	 */

	
	/*-
	 * Memorandum 2 (Sat, 20 Jul 2019 11:33:56 +0900)
	 * 
	 * When a Scheme invokable is called from SwingUtilities#invokeLater(),
	 * NullPointerException occurs sometimes. The stack trace is following : 
	 *
	 * <pre>
	 *   java.lang.NullPointerException
	 *   at gnu.expr.PrimProcedure.decodeType(PrimProcedure.java:404)
	 *   at gnu.expr.PrimProcedure.<init>(PrimProcedure.java:371)
	 *   at gnu.kawa.reflect.ClassMethods.getMethods(ClassMethods.java:136)
	 *   at gnu.kawa.reflect.ClassMethods.apply(ClassMethods.java:252)
	 *   at gnu.kawa.functions.GetNamedPart.getMemberPart(GetNamedPart.java:120)
	 *   at gnu.kawa.functions.GetNamedPart.getNamedPart(GetNamedPart.java:106)
	 *   at gnu.kawa.functions.GetNamedPart.apply2(GetNamedPart.java:46)
	 *   at atInteractiveLevel-93$frame.lambda14(pulsar-basic-framework.scm:612)
	 * </pre>
	 *  The reason this NPE occurs is the passed "lang" argument sometimes becomes null
	 *
	 * <pre>
	 *	    public static Type decodeType(Type javaType,
	 *	                                  String[] annotTypes, int annotIndex,
	 *		                                  ParameterizedType parameterizedType,
	 *		                                  Language lang) {
	 *		        String annotType = annotTypes != null && annotTypes.length > annotIndex
	 *		            ? annotTypes[annotIndex] : null;
	 *		        return lang.decodeType(javaType, annotType, parameterizedType);
	 *		    }
	 * </pre>
	 * 
	 * Then I found that setting current language by Language.setCurrentLanguage( language ) 
	 * effectively suppresses the NPE.
	 *  
	 * I am pretty sure this is not supposed to be. 
	 * But I don't know how to manage this problem for some years. 
	 */

	protected static void specialInit(Scheme newScheme) {
		Environment.setCurrent( newScheme.getEnvironment() );
		Language.setCurrentLanguage( newScheme );
		{
			String threadName = Thread.currentThread().getName();
			System.err.println( threadName +
				"(Environment.getCurrent() == newScheme.getEnvironment() ) : " + 
				( Environment.getCurrent() == newScheme.getEnvironment() ) 
					);
			System.err.println( threadName +
				"( Language.getDefaultLanguage() == newScheme ) : " + 
				( Language.getDefaultLanguage() == newScheme ) 
					);
		}
	}
	
	public Scheme newScheme() {
		try {
			// 1. This initializes Secretary Message Queue's thread.
			Scheme scheme = executeSecretarially( new SecretaryMessage.NoThrow<Scheme,Scheme>() {
				@Override
				public Scheme execute0(Scheme scheme, Object[] args) {
					Scheme newScheme = new Scheme();
					setExecutive( newScheme );
					specialInit( newScheme );
					return newScheme;
				}
			});
			
			// 2. (in most case ) this initializes main-thread
			specialInit(scheme);
			
			// 3. This initializes AWT-EventQueue's thread.
			SwingUtilities.invokeLater( new Runnable() {
				@Override
				public void run() {
					specialInit(scheme);
				}
			});
			return scheme;
		} catch (Throwable e) {
			throw new RuntimeException(e);
		}
	}
	public void setScheme( Scheme newScheme ) {
		try {
			executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
				@Override
				public void execute0(Scheme resource, Object[] args) {
					setExecutive( newScheme );
				}
			});
		} catch (Throwable e) {
			throw new RuntimeException(e);
		}
	}
}

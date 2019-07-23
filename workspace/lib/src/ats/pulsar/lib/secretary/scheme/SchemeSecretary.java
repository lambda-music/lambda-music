package ats.pulsar.lib.secretary.scheme;

import java.util.ArrayList;
import java.util.List;

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

	public static final void initializeSchemeForCurrentThreadStatic( Scheme scheme ) {
		Environment.setCurrent( scheme.getEnvironment() );
		Language.setCurrentLanguage( scheme );
		{
			String threadName = Thread.currentThread().getName();
			System.err.println( threadName +
				"(Environment.getCurrent() == newScheme.getEnvironment() ) : " + 
				( Environment.getCurrent() == scheme.getEnvironment() ) 
					);
			System.err.println( threadName +
				"( Language.getDefaultLanguage() == newScheme ) : " + 
				( Language.getDefaultLanguage() == scheme ) 
					);
		}
	}
	
	public final void initializeSchemeForCurrentThread() {
		initializeSchemeForCurrentThreadStatic( this.getScheme() );
	}
	
	static class InitializerEntry {
		Object parent;
		SecretaryMessage.NoReturnNoThrow<Scheme> message;
		public InitializerEntry(Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message) {
			super();
			this.parent = parent;
			this.message = message;
		}
	}
	
	List<SchemeSecretary.InitializerEntry> newSchemeInitializerList = new ArrayList<>();
	
	/**
	 * This method registers a specified initializer.
	 * @see #invokeSchemeInitializers(Object)
	 */
	public void registerSchemeInitializer( Object parent, SecretaryMessage.NoReturnNoThrow<Scheme> message ) {
		this.newSchemeInitializerList.add( new SchemeSecretary.InitializerEntry( null, message ) );
	}
	/**
	 * This method unregisters a specified initializer.
	 * @see #invokeSchemeInitializers(Object)
	 */
	public void unregisterSchemeInitializer( SecretaryMessage.NoReturnNoThrow<Scheme> message ) {
		this.newSchemeInitializerList.removeIf( e->e.message == message );
	}

	/**
	 * This method invokes registered initializers to which the given condition match.
	 *  
	 * @param parent
	 *     Passing null to invoke all initializers or passing parent object to invoke 
	 *     only specific initializers. 
	 * 
	 *     If the parent argument equals to the parent of a registered initializer, the 
	 *     initializer will be invoked. If the parent argument is null, all initializers
	 *     will be invoked.
	 */
	public void invokeSchemeInitializers( Object parent ) {
		for ( SchemeSecretary.InitializerEntry e : newSchemeInitializerList ) {
			if ( parent == null || e.parent == parent )
				executeSecretarially( e.message );
		}
	}

	{
		registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0(Scheme scheme, Object[] args) {
				// 3. This initializes Secretary Message Queue's thread.
				// // 4. (in most case ) this initializes main-thread
				initializeSchemeForCurrentThreadStatic( scheme );
			}
		} );

		registerSchemeInitializer( null, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0(Scheme scheme, Object[] args) {
				SwingUtilities.invokeLater( new Runnable() {
					@Override
					public void run() {
						// 5. This initializes AWT-EventQueue's thread.
						initializeSchemeForCurrentThreadStatic(scheme);
					}
				});
			}
		});
	}
	
	public void newScheme() {
		try {
			// 1. Create a new scheme object.
			executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
				@Override
				public void execute0(Scheme scheme, Object[] args) {
					Scheme newScheme = new Scheme();
					setExecutive( newScheme );
				}
			});
			// 2. Execute all the initializers.
			invokeSchemeInitializers( null );
		} catch (Throwable e) {
			throw new RuntimeException(e);
		}
	}
	
	void setScheme( Scheme newScheme ) {
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

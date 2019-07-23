/*
 * Pulsar-Sequencer written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Pulsar-Sequencer. 
 * 
 * Pulsar-Sequencer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Pulsar-Sequencer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
 */

package ats.pulsar.lib.kawa.lambda.legacy;

import ats.pulsar.lib.secretary.Invokable;
import gnu.expr.Language;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;

/**
 * This class implements invocation of procedures of a Kawa scheme.
 * 
 * In this application, every exceptions are merely printed in standard error
 * stream and these exceptions are not handled. When any exception are thrown,
 * the processing just continues and will not interrupt.
 * 
 * This behavior is currently intentional because many processing are done in
 * multithreading and also time-critical since this application works with
 * JACKaudio.
 * 
 * These exceptions should be displayed not only in the standard error stream,
 * but also in balloons in notification area,etc something which are easily
 * visible to users. Some new systems are expected to be developed in future.
 * 
 * @author ats
 */
public class InvokableSchemeProcedure implements Invokable {
	/**
	 * This field specifies an object to be synchronized with.
	 * This is necessary since Kawa is not very good at multithreading. 
	 */
	private final Object syncObj;
	
	/**
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
	private final Environment environment;
	@SuppressWarnings("unused")
	private final Language language;
	
	/*-
	 * (Sat, 20 Jul 2019 11:33:56 +0900)
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
	
	/**
	 * The invokable to invoke.
	 */
	private final Procedure procedure;

	public InvokableSchemeProcedure(Object syncObj, Environment environment, Language language, Procedure procedure) {
		this.syncObj = syncObj;
		this.environment = environment;
		this.language = language;
		this.procedure = procedure;
	}


	protected InvokableSchemeProcedure(Object syncObj, Environment environment, Procedure procedure) {
		this.syncObj = syncObj;
		this.environment = environment;
		this.language = Language.getDefaultLanguage();
		this.procedure = procedure;
	}

	@Override
	public Object invoke(Object... args) {
		synchronized (syncObj) {
			try {
				Environment.setCurrent( this.environment );
				@SuppressWarnings("unused")
				Scheme scheme = ((Scheme)syncObj);
//				Language.setCurrentLanguage( language ); // ADDED (Sat, 20 Jul 2019 11:25:21 +0900)
				// REMOVED (Mon, 22 Jul 2019 10:09:39 +0900)
				return procedure.applyN( args );
			} catch (Throwable e) {
				LegacyKawaUtils.logError("SchemeInvokableProcedure", e);
				throw new RuntimeException(e);
			}
		}
	}
}

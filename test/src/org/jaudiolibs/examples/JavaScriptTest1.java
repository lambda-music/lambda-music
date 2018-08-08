package org.jaudiolibs.examples;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import jdk.nashorn.api.scripting.ScriptObjectMirror;

public class JavaScriptTest1 {
	public static void main(String[] args) throws ScriptException, NoSuchMethodException {
		ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
		Object o = engine.eval("({ hello : function(s) { print( 'hello world ' + s ); }, bar : null, })");

		
		System.out.println( 
				((ScriptObjectMirror)
					((ScriptObjectMirror) o).get( "hello" )
				).call(null, "hello")
		);
		
	}
}

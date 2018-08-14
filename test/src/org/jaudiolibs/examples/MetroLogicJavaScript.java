package org.jaudiolibs.examples;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import ats.metro.Metro;
import ats.metro.MetroLogic;
import ats.metro.MetroLogicHandle;
import ats.metro.MetroMasterLogic;
import ats.metro.MetroMidiEventBuffer;
import jdk.nashorn.api.scripting.ScriptObjectMirror;

final class MetroLogicJavaScript extends MetroMasterLogic.Default {
	
	private String clientName;
	private List<String> outputPortNameList;
	private Set<Entry<String, String>> optionalConnection;
	private ScriptObjectMirror mirror;
	
	public class MetroAPI {
		public void spawn( double offset, ScriptObjectMirror subProcessor ) {
			MetroLogicJavaScript.this.handle.spawn(offset, new MetroLogic.Default() {
				@Override
				public boolean processBuffer( MetroMidiEventBuffer buf ) {
					// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );
					Object result = subProcessor.call( null , buf );
					return checkReturnValue( result );
				}
			});
		}
	}
	private MetroAPI metroAPI = new MetroAPI();
	private ScriptObjectMirror processor;
	private ScriptEngine engine;


	@Override
	public String clientName() {
		return this.clientName;
	}

	@Override
	public Set<Entry<String, String>> optionalConnection() {
		return this.optionalConnection;
	}

	@Override
	public List<String> outputPortNameList() {
		return this.outputPortNameList;
	}
	
	public void setHandle(MetroLogicHandle handle) {
		super.setLogicHandle( handle );
	}

	public MetroLogicJavaScript( Path path ) throws ScriptException, IOException {
		String script = new String(Files.readAllBytes( path ));
		
		engine = new ScriptEngineManager().getEngineByName("nashorn");
		engine.put( "metro", metroAPI  );
		
		this.mirror = (ScriptObjectMirror) engine.eval( "(" + script + ")" );
		this.clientName = (String) mirror.get( "clientName" );
		
		// Retrieve outputPortNameList
		{
			ScriptObjectMirror som = (ScriptObjectMirror) mirror.get( "outputPortNameList" );
			if ( som == null)
				this.outputPortNameList = Arrays.asList( new String[] {} );
			else
				this.outputPortNameList = Arrays.asList( som.to(String[].class ) );
		}
		
		// Retrieve optionalConnection
		{
			ScriptObjectMirror som = (ScriptObjectMirror) mirror.get( "optionalConnection" );
			if ( som == null)
				this.optionalConnection = new LinkedHashSet<Entry<String,String>>();
			else if ( som.isArray() ) {
				LinkedHashMap<String,String> m = new LinkedHashMap<String,String>();
				for ( ScriptObjectMirror som2 : som.to( ScriptObjectMirror[].class ) ) {
					String[] ss = som2.to( String[].class );
					if ( ss.length < 2 )
						throw new RuntimeException( "optionalConnection must contain an array of arrays that consist two strings.[ ['foo','bar'],[ 'bum', 'buzz' ]], " );
						m.put( ss[0] , ss[1] );
				}
				this.optionalConnection = m.entrySet();
			} else {
				throw new RuntimeException( "optionalConnection should be an array object." );
			}
			
		}

		// Retrieve outputPortNameList
		{
			ScriptObjectMirror som = (ScriptObjectMirror) mirror.get( "process" );
			if ( som == null) {
				throw new RuntimeException( "a processor method must be specified." );
			} else
				this.processor = som;
		}
		
	}

	@Override
	public void initialize() {
	}
	
	boolean checkReturnValue( Object result ) {
		if ( ( result instanceof Boolean ) && ((Boolean)result) == false ) {
			return false;
		} else {
			return true; 
		}
	}

	@Override
	public boolean processBuffer( MetroMidiEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );
		Object result = this.processor.call( null , buf );
		return checkReturnValue( result );
	}

	public static void main(String[] args) throws ScriptException, IOException, URISyntaxException {
        MetroLogicJavaScript logic = new MetroLogicJavaScript( new File( "test.js" ).toPath() );
		Metro.startClient( logic );

	}

}
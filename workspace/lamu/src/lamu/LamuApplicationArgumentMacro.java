package lamu;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class LamuApplicationArgumentMacro {
    List<String> input;
    List<String> output = new ArrayList<>();
    public LamuApplicationArgumentMacro( List<String> input ) {
        this.input = input;
    }
    public List<String> getInput() {
        return input;
    }
    public List<String> getOutput() {
        return output;
    }
    public String[] getOutputAsArray() {
        return output.toArray( new String[ output.size() ] );
    }


    static final HashMap<String,LamuApplicationArgumentCommand> map = new HashMap<>();
    public static void register( LamuApplicationArgumentCommand e ){
        map.put( e.getName(), e );
    }
    
    /**
     * An abstract class which represents a macro definition. 
     */
    public static abstract class LamuApplicationArgumentCommand {
    	private String name;
		public LamuApplicationArgumentCommand(String name) {
			this.name = name;
		}
		public String getName() {
			return name;
		}
    	
		/**
    	 * Returns the macro-expanded elements.
    	 * @param namedArgs
    	 *    the named arguments which are passed in the current command-line arguments. 
    	 * @return
    	 *    the macro-expanded elements. 
    	 */
    	public abstract List<String> execute( Map<String,LamuApplicationNamedArgument> namedArgs );
    }

    /**
     * This class defines the basic facilities to this macro system.
     * <h3>Example1</h3>
     * <pre>{@code
     * [ "hello", "foo", "$VAR{ --open @ --default }", "bar" ]
     * }</pre>
     * <h3>Example2</h3>
     * <pre>{@code
     * [ "hello", "foo", "$!VAR{ --open foo/bar/text.txt }", "bar" ]
     * }</pre>
     * 
     * <h3>Variables</h3>
     * When a token starts with a dollar `$` mark, then the token is treated as a variable.
     * A variable is replaced with the corresponding named arguments. Please refer the overview.
     */
    static class DefaultMactroElement extends LamuApplicationArgumentCommand {
    	public void execute( List<String> input, List<String> output ) {
    		String token=null;
    		for ( Iterator<String> i = input.iterator();;) {
    			if ( token != null ) {
    				if ( map.containsKey( token ) ) {
    					LamuApplicationArgumentCommand e = map.get( token );
    					HashMap<String,LamuApplicationNamedArgument> nargs = new HashMap<>();
    					while ( i.hasNext() ) {
    						token=i.next();
    						if ( token.startsWith( "--" ) ) {
    							LamuApplicationNamedArgument na = new LamuApplicationNamedArgument( token );
    							nargs.put( na.getKey(), na );
    						} else {
    							break;
    						}
    					}
    					output.addAll( e.execute( nargs ) );
    					continue;
    				} else {
    					output.add( token );
    				}
    			}
    			if ( i.hasNext() ) {
    				token = i.next();
    			} else {
    				break;
    			}
    		}
    	}

    	//public static List<String> stringSplitter( String value ) {
    	//	List<String> in = Arrays.asList( value.trim().split( "[\\s]+" ) );
    	//	ArrayList<String> out = new ArrayList<>();
    	//	for ( Iterator<String> i =in.iterator(); i.hasNext(); ) {
    	//		String token = i.next().trim();
    	//		// strip the quotations
    	//		if ( token.startsWith( "\"" ) && token.endsWith( "\"" ) ) {
    	//			token = token.substring( 1, token.length()-2 );
    	//		}
    	//		out.add(token);
    	//	}
    	//	return out;
    	//}

    	public static ArrayList<String> splitString( String value ) {
    		return new ArrayList<>( Arrays.asList( value.trim().split( "[\\s]+" ) ) );
    	}
    	public static LamuApplicationArgumentCommand parseElement(String value) {
    		ArrayList<String> list = splitString( value );
    		if ( list.size() == 1 && list.get(0).trim().equals( "" ) ) {
    			return null;
    		}
    		String macroName = list.remove(0);
    		List<String>  macroContent = list;
    		return new DefaultMactroElement( macroName , macroContent );
    	}

    	public void load( String s ) {
    		String[] strings = s.split( "\n" );
    		for ( int i=0; i<strings.length; i++ ) {
    			LamuApplicationArgumentCommand e = parseElement( strings[i] );
    			if ( e != null )
    				register( e );
    		}
    	}

    	List<String> macroContent;
    	public DefaultMactroElement(String name, List<String> macroContent) {
    		super(name);
    		this.macroContent = macroContent;
    	}
    	@Override
    	public List<String> execute(Map<String, LamuApplicationNamedArgument> namedArgs) {
    		ArrayList<String> result = new ArrayList<String>();
    		for ( Iterator<String> i = macroContent.iterator(); i.hasNext(); ) {
    			String token = i.next().trim();

    			if ( token.startsWith( "$" ) ) {
    				// if the current token is a variable; replace the token with the corresponding value.
    				token = token.substring(1);

    				// the default value as the substitutional string for the variable token.
    				String subst = "@";

    				// this enables negation of checking existence of the namedArgs.
    				boolean expectationForContains = true;
    				if ( token.startsWith( "!" ) ) {
    					token = token.substring(1);
    					expectationForContains = false;
    				}

    				// 
    				int idx0 = token.indexOf( "{" );
    				int idx1 = token.indexOf( "}" );
    				if ( 0<=idx0 && 0<=idx1 && idx0 < idx1 ) {
    					subst = token.substring(idx0+1, idx1).trim(); 
    					token = token.substring(0,idx0).trim(); 
    				}

    				boolean contains = namedArgs.containsKey(token);
    				if ( expectationForContains  ==  contains ) {
    					ArrayList<String> substList = splitString( subst );
    					for ( Iterator<String> j = substList.iterator(); j.hasNext(); ) {
    						String substToken = j.next();
    						if ( substToken.equals("@")) {
    							result.add( namedArgs.get( token ).getValue() );
    						} else {
    							result.add( substToken );
    						}
    					}
    				}
    			} else {
    				// Otherwise, simply add the current token.
    				result.add( token );
    			}
    		}
    		return result;
    	}
    }

    
}

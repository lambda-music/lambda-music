package lamu;

import gnu.expr.SourceName;
import gnu.mapping.Procedure0;

/**
 * This class demonstrates so-called 'mangling' which is described in
 * the <a href="https://www.gnu.org/software/kawa/Mangling.html">documentation</a>.  
 *<pre> 
 * (import (lamu hello))
 * => (hello-world-91) is available
 * 
 * ((lamu.hello):hello-world-91 )
 * => error
 * 
 * ((lamu.hello):fooBar )
 * => available
 * 
 * ((lamu.hello):foo-bar )
 * => error
 *
 * ((lamu.hello):hello-world-foo-bar )
 * => available
 * 
 * ((lamu.hello):hello-world-error )
 * => error
 *
 * ((lamu.hello):hello-world-available )
 * => available
 * 
 * </pre>
 * 
 */
public class hello {
    @SourceName( name = "hello-world-91" )
    public static Procedure0 helloWorld = new Procedure0() {
        @Override
        public Object apply0() throws Throwable {
            return "hello world";
        }
    }; 
    public Procedure0 fooBar = new Procedure0() {
        @Override
        public Object apply0() throws Throwable {
            return "hello world";
        }
    };
    
    public String helloWorldFooBar() {
        return "foo";
    }
    
    public final Procedure0 fieldNameIsIgnored = new Procedure0("hello-world-error") {
        @Override
        public Object apply0() throws Throwable {
            return "hello world foo bar";
        }
    };
    
    public Procedure0 getHelloWorldAvailable() {
        return fieldNameIsIgnored;
    }
    
    public String doHello( String a ) {
        return a + "hello";
    }
}

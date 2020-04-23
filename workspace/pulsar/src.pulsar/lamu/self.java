package lamu;

import gnu.expr.Compilation;
import gnu.mapping.Procedure0;
import kawa.lang.Translator;

public class self {
    public final Procedure0 proc = new Procedure0("hello-world-error") {
        @Override
        public Object apply0() throws Throwable {
            System.out.println(((Translator) Compilation.getCurrent()));
//            System.out.println(((Translator) Compilation.getCurrent()).getCurrentSyntax());
            return "hello world foo bar";
        }
    };
    
    public Procedure0 getHello() {
        return proc;
    }

}

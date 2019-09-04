package pulsar.lib;

import gnu.mapping.Procedure;
import gnu.mapping.Procedure0;

public class gui {
    public gui() {
    }
    public Procedure gui_hello = new Procedure0() {
        @Override
        public Object apply0() throws Throwable {
            return "hello world";
        }
    }; 
}

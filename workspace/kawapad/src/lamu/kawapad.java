package lamu;

import gnu.expr.SourceName;
import gnu.mapping.Procedure;
import kawapad.Kawapad;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;

public class kawapad {
    @SourceName(name ="current-kawapad")
    public Procedure currentKawapad = new MultipleNamedProcedure0() {
        @Override
        public Object apply0() throws Throwable {
            return Kawapad.getCurrent();
        }
    };
    @SourceName(name ="current-kawapad-present?")
    public Procedure currentKawapadPresent = new MultipleNamedProcedure0() {
        @Override
        public Object apply0() throws Throwable {
            return Kawapad.isPresent();
        }
    };
}

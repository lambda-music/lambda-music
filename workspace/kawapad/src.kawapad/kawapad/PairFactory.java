package kawapad;

import gnu.lists.LList;
import gnu.lists.Pair;

public interface PairFactory {
    Pair make( Object car, Object cdr );

    public static LList makeList( PairFactory factory, java.util.List vals ) {
        java.util.Iterator e      = vals.iterator();
        LList              result = LList.Empty;
        Pair               last   = null;
        while (e.hasNext()) {
            Pair pair = factory.make( e.next(), LList.Empty );
            if (last == null)
                result = pair;
            else
                last.setCdr( pair );
            last = pair;
        }
        return result;
    }
}

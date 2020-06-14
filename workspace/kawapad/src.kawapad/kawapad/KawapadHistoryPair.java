package kawapad;

import gnu.lists.Pair;

public class KawapadHistoryPair extends Pair implements KawapadHistoryObject{
    public KawapadHistoryPair(Pair pair) {
        this.setCar(pair.getCar());
        this.setCdr(pair.getCdr());
    }
    
    public KawapadHistoryPair( Object car, Object cdr ) {
        this.setCar(car);
        this.setCdr(cdr);
    }

    public static final PairFactory FACTORY = new PairFactory() {
        @Override
        public Pair make(Object car, Object cdr) {
            return new KawapadHistoryPair( car, cdr );
        }
    };

}

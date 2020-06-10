package metro;

class MetroConstantSequenceFactory implements MetroSequenceFactory {
    private final MetroSequence sequence;
    MetroConstantSequenceFactory(MetroSequence sequence) {
        super();
        this.sequence = sequence;
    }
    @Override
    public MetroSequence createSequence() {
        return sequence;
    }
    @Override
    public String toString() {
        return String.format( "(const-sequence-factory value: %s)" , sequence );
    }
}
public interface MetroSequenceFactory {
    public static MetroSequenceFactory createConstant(MetroSequence sequence) {
        return new MetroConstantSequenceFactory(sequence);
    }
    MetroSequence createSequence();
}

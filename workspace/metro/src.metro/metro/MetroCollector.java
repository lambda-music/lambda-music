package metro;

/**
 * (Fri, 07 Aug 2020 08:59:04 +0900) This {@link MetroCollector} formerly
 * be called MetroCollector currently has no specific job. At the time of
 * creation, this is used to collect multiple values when
 * {@link PulsarNoteListParser} is called. But in the later time, the values are
 * already received by {@link MetroBufferedMidiReceiver} in the case.
 * 
 * It could be removed. Somehow it can be used for other purposes in future; I
 * left as it is.
 *
 * @param <T>
 */
@FunctionalInterface
public interface MetroCollector<T> {
    void collect( T value );
    public static final MetroCollector VOID = new VoidCollector<>();
}

class VoidCollector<T> implements MetroCollector<T> {
    @Override
    public void collect(T value) {
    }
}

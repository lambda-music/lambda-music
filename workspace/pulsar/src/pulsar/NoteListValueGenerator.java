package pulsar;

public interface NoteListValueGenerator<T> {
    public static final class Default<T> implements NoteListValueGenerator<T> {
        final T value;
        public Default(T value) {
            super();
            this.value = value;
        }
        @Override
        public T generate() {
            return value;
        }
    }
    NoteListValueGenerator<?> NULL = new Default( null );
    T generate();
}

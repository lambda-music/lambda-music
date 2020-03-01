package lamu;

public interface NoteListValueConverter<T> {
    public static final class Default implements NoteListValueConverter {
        @Override
        public Object convert(Object value) {
            return value;
        }
    }
    public static final NoteListValueConverter THRU = new Default();
    
    T convert( Object value );
}

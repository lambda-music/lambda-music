package pulsar;

import gnu.mapping.Symbol;

public class DirectNoteListParser {
    public static final Symbol TYPE = Symbol.valueOf( "type" );
    static abstract class NotationValueConverter {
        abstract Object convert( Object value );
    }
    static class NotationField {
        Symbol name;
        Object defaultValue; // s2j only
        NotationValueConverter j2s; // java   -> scheme value conversion
        NotationValueConverter s2j; // scheme -> java   value conversion
    }
    public static class NotationDefinition {
        Symbol type;
        NotationField[] fields;
        public NotationDefinition(NotationField[] fields) {
            super();
            this.fields = fields;
        }
    }
}

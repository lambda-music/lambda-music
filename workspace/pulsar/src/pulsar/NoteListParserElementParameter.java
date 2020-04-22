package pulsar;

import gnu.mapping.Symbol;
import lamu.lib.evaluators.SchemeValues;

public abstract class NoteListParserElementParameter {
    public abstract String getShortName();
    public abstract String getLongName();
    public abstract String getType();
    public abstract String getDefaultValue();
    public abstract String getDescription();

    public static class Default extends NoteListParserElementParameter{
        private String shortName;
        private String longName;
        private String type;
        private String defaultValue;
        private String description;
        public Default() {
        }
        public Default(Symbol shortName, Symbol longName, String type, String defaultValue, String description) {
            this( 
                SchemeValues.toString(shortName),
                SchemeValues.toString(longName),
                type, defaultValue, description );
        }
        public Default(String shortName, String longName, String type, String defaultValue, String description) {
            super();
            this.shortName = shortName;
            this.longName = longName;
            this.type = type;
            this.defaultValue = defaultValue;
            this.description = description;
        }
        @Override
        public String getShortName() {
            return shortName;
        }

        public void setShortName(String shortName) {
            this.shortName = shortName;
        }

        @Override
        public String getLongName() {
            return longName;
        }

        public void setLongName(String longName) {
            this.longName = longName;
        }

        @Override
        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        @Override
        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }
        @Override
        public String getDefaultValue() {
            return defaultValue;
        }
        public void setDefaultValue(String defaultValue) {
            this.defaultValue = defaultValue;
        }
    }
}

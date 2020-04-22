package lamu.lib.helps;

import java.util.List;

import gnu.mapping.Procedure;
import lamu.lib.kawautils.procedures.MultipleNamed;

public interface LamuDocumentCondition {
    boolean check( LamuDocument bean );
    static final class CateogoryCondition implements LamuDocumentCondition {
        private final String category;
        public CateogoryCondition(String category) {
            if ( category == null )
                throw new NullPointerException();
            this.category = category;
        }
        @Override
        public boolean check(LamuDocument d) {
            return category.equals(d.getCategory()); 
        }
    }
    static final class NameCondition implements LamuDocumentCondition {
        private final String name;
        public NameCondition(String name) {
            if ( name == null )
                throw new NullPointerException();
            this.name = name;
        }
        @Override
        public boolean check(LamuDocument d) {
            return d.getNames().contains( name );
        }
    }
    static final class NameListCondition implements LamuDocumentCondition {
        private final List<String> nameList;
        public NameListCondition(List<String> nameList) {
            if ( nameList == null )
                throw new NullPointerException();
            this.nameList = nameList;
        }
        @Override
        public boolean check(LamuDocument d) {
            for ( String s : nameList ) {
                if ( d.getNames().contains( s ) )
                    return true;
            }
            return false;
        }
    }


    static LamuDocumentCondition createConditionByCategory( String category ) {
        return new CateogoryCondition(category);
    }

    static LamuDocumentCondition createConditionByName( String name ) {
        return new NameCondition(name);
    }

    static LamuDocumentCondition createConditionByNameList( List<String> nameList ) {
        return new NameListCondition( nameList );
    }

    static LamuDocumentCondition createConditionByProcedure( Procedure proc ) {
            if ( proc instanceof MultipleNamed )  {
                return new NameListCondition( ((MultipleNamed)proc).getNames() );
            } else {
                // TODO !!!!!!!!!
                throw new RuntimeException( "TEST : should not come here exception" );
    //            return new NameCondition( proc.getName() );
            } 
        }
}
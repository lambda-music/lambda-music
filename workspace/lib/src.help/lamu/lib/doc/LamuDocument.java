package lamu.lib.doc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.scheme.SchemeDocument;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.proc.MultipleNamed;

public class LamuDocument implements Cloneable  {
    private static final ArrayList<LamuDocument> all =new ArrayList<>();
    public static interface Condition {
        boolean check( LamuDocument bean );
    }
    private static final class CateogoryCondition implements Condition {
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
    private static final class NameCondition implements Condition {
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
    private static final class NameListCondition implements Condition {
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
    public static void add( LamuDocument d ) {
        synchronized( all ) {
            all.add( 0, d );
        }
    }

    public static List<LamuDocument> get() {
        synchronized( all ) {
            return new ArrayList<LamuDocument>( all );
        }
    }
    public static List<LamuDocument> get( Condition condition ) {
        synchronized( all ) {
            ArrayList<LamuDocument> result =  new ArrayList<LamuDocument>();
            for ( LamuDocument d : all ) {
                if ( condition.check(d)  ) {
                    result.add(d);
                }
            }
            return result;
        }
    }
    static void remove( Condition condition ) {
        synchronized( all ) {
            for( Iterator<LamuDocument> i = all.iterator(); i.hasNext(); ) {
                LamuDocument d = i.next();
                if ( condition.check(d)) {
                    i.remove();
                }
            }
        }
    }
    public static Condition createConditionByCategory( String category ) {
        return new CateogoryCondition(category);
    }
    public static Condition createConditionByName( String name ) {
        return new NameCondition(name);
    }
    public static Condition createConditionByNameList( List<String> nameList ) {
        return new NameListCondition( nameList );
    }
    public static Condition createConditionByProcedure( Procedure proc ) {
        if ( proc instanceof MultipleNamed )  {
            return new NameListCondition( ((MultipleNamed)proc).getNames() );
        } else {
            // TODO !!!!!!!!!
            throw new RuntimeException( "TEST : should not come here exception" );
//            return new NameCondition( proc.getName() );
        } 
    }
    
    public static List<String> getAllAvailableCategory(){
        synchronized( all ) {
            ArrayList<String> result = new ArrayList<>();
            for ( LamuDocument d : all ) {
                if ( ! result.contains( d.getCategory() ) ) {
                    result.add( d.getCategory() );
                }
            }
            return result;
        }
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    private static final int MAX_SERIES_COUNT = 16;
    public static class Param implements Cloneable {
        List<String> names;
        String type;
        String defaultValue;
        boolean variable=false;
        String description;
        public Param(List<String> names, String type, String defaultValue, boolean variable, String description) {
            super();
            this.names = names;
            this.type = type;
            this.defaultValue = defaultValue;
            this.variable = variable;
            this.description = description;
        }
        public String getName() {
            if ( getNames().isEmpty() )
                throw new IllegalArgumentException();
            return getNames().get( 0 );
        }

        public List<String> getNames() {
            return names;
        }
        public void setNames(List<String> names) {
            this.names = names;
        }
        public String getType() {
            return type;
        }
        public void setType(String type) {
            this.type = type;
        }
        public String getDefaultValue() {
            return defaultValue;
        }
        public void setDefaultValue(String defaultValue) {
            this.defaultValue = defaultValue;
        }
        public boolean isVariable() {
            return variable;
        }
        public void setVariable(boolean variable) {
            this.variable = variable;
        }
        public String getDescription() {
            return description;
        }
        public void setDescription(String description) {
            this.description = description;
        }
        public Param processArguments(Object[] args) {
            Param out = new Param(
                this.getNames(),
                this.getType(),
                this.getDefaultValue(),
                this.isVariable(),
                String.format( 
                    this.getDescription(), args ));
            return out;
        }
        @Override
        public Param clone() {
            // TODO Auto-generated method stub
            try {
                Param result = (Param) super.clone();
                result.names = new ArrayList<>( this.names );
                return result;
            } catch (CloneNotSupportedException e) {
                throw new InternalError( e );
            }
        }
    }
    
    public LamuDocument() {
        LamuDocument.add( this );
    }
    
    // this method could be purged
    LamuDocument(
            String category,
            List<String> names, 
            String parameterDescription, String returnValueDescription,
            String shortDescription, String longDescription) 
    {
        super();
        this.category = category;
        if ( names == null || names.isEmpty() )
            throw new IllegalArgumentException();
        this.names = names;
        
        this.parameterDescription = parameterDescription;
        this.returnValueDescription = returnValueDescription;
        this.shortDescription = shortDescription;
        this.longDescription = longDescription;
        
    }
    

    private static ArrayList<Param> deepCopyParamList( ArrayList<Param> list ) {
        ArrayList<Param> resultList = new ArrayList<>();
        for ( Param p : list ) {
            resultList.add( p.clone() );
        }
        return resultList;
    }
    private static ArrayList<ArrayList<Param>> deepCopyParamListList( ArrayList<ArrayList<Param>> inListList ) {
        ArrayList<ArrayList<Param>> outListList = new ArrayList<>();
        for ( ArrayList<Param> inList : inListList ) {
            outListList.add( deepCopyParamList( inList ) );
        }
        return outListList;
    }
    
    @Override
    public LamuDocument clone() {
        try {
            /*
             * At this point, it is unknown that what class this object is going to be;
             * though, Object#clone() supports duplicating unknown class.
             */
            LamuDocument result = (LamuDocument)super.clone();
            result.names = this.names == null ? null : new ArrayList<>( this.names );
            result.parameterListList = deepCopyParamListList( this.parameterListList );
            return result;
        } catch (CloneNotSupportedException e) {
            throw new InternalError(e);
        }
    }

    private boolean isVisible=true;
    private String category;
    private List<String> names = Collections.EMPTY_LIST;
    private String parameterDescription;
    private ArrayList<ArrayList<Param>> parameterListList = new ArrayList<>();
    private String returnValueDescription;
    private String shortDescription;
    private String longDescription;
    private LamuDocumentSynopsisFormatter synopsisFormatter = ProcedureSynopsisFormatter.getInstance();
    
    {
        // do this in order to display empty parameter correctly
        // (Sat, 18 Jan 2020 14:37:26 +0900)
        parameterListList.add( new ArrayList<>() );
    }
    
    public void setVisible(boolean isVisible) {
        this.isVisible = isVisible;
    }
    public boolean isVisible() {
        return isVisible;
    }
    
    public String getCategory() {
        return category;
    }
    public void setCategory(String category) {
        this.category = category;
    }

    public String getName() {
        if ( getNames() == null || getNames().isEmpty() )
            throw new IllegalArgumentException();
        return getNames().get( 0 );
    }

    public List<String> getNames() {
        return names;
    }
    public LamuDocument setNames(String ... names) {
        this.setNames( Arrays.asList( names ) );
        return this;
    }
    public LamuDocument setNames(List<String> names) {
        if ( names.isEmpty() )
            throw new IllegalArgumentException();
        this.names = names;
        return this;
    }

    public String getParameterDescription() {
        return parameterDescription;
    }
    public void setParameterDescription(String parameterDescription) {
        this.parameterDescription = parameterDescription;
    }
    public void addParameter( int seriesNo, Param param ){
        this.getParameterList( seriesNo ).add( param );
    }
    
    public void addParameter( int seriesNo, String name, String type, String defaultValue, boolean isVariable, String description ){
        this.addParameter( seriesNo, new Param( Arrays.asList( name ), type, defaultValue, isVariable, description ) );
    }
    public void addParameter( int seriesNo, List<String> names, String type, String defaultValue, boolean isVariable, String description ){
        this.addParameter( seriesNo, new Param( names, type, defaultValue, isVariable, description ) );
    }
    
    private static final void ensureSize( ArrayList list, int size ) {
        list.ensureCapacity( size );
        while ( list.size() < size ) {
            list.add( new ArrayList<>());
        }
    }
    
    public List<Param> getParameterList( int seriesNo ) {
        if ( seriesNo < 0 || MAX_SERIES_COUNT <= seriesNo ) {
            throw new IllegalArgumentException( "an invalid number of series number (" + seriesNo + ")" );
        }
        ensureSize( this.parameterListList, seriesNo + 1 );
        return this.parameterListList.get( seriesNo );
    }
    public int getParameterListCount() {
        return this.parameterListList.size();
    }
    public String getReturnValueDescription() {
        return returnValueDescription;
    }
    public void setReturnValueDescription(String returnValueDescription) {
        this.returnValueDescription = returnValueDescription;
    }
    public String getShortDescription() {
        return shortDescription;
    }
    public void setShortDescription(String shortDescription) {
        this.shortDescription = shortDescription;
    }
    public String getLongDescription() {
        return longDescription;
    }
    public void setLongDescription(String longDescription) {
        this.longDescription = longDescription;
    }
    public LamuDocumentSynopsisFormatter getSynopsisFormatter() {
        return synopsisFormatter;
    }
    public void setSynopsisFormatter(LamuDocumentSynopsisFormatter synopsisFormatter) {
        this.synopsisFormatter = synopsisFormatter;
    }

    public LamuDocument processArguments( Object ... args ) {
        return processArguments0( this, args );
    }
    
    public String getSynopsis( int seriesNo ) {
        return synopsisFormatter.getSynopsis( this, seriesNo );
    }

    
//  @Override
//  public abstract String formatSynopsis();

    static LamuDocument processArguments0( LamuDocument in, Object ... args ) {
        // See comment in DescriptiveBean#clone() 
        LamuDocument out = in.clone();
        
        out.setParameterDescription( String.format( in.getParameterDescription(), args ));

        for ( ArrayList<Param> out_list : out.parameterListList ) {
            for ( ListIterator<Param> out_iter = out_list.listIterator(); out_iter.hasNext(); ) {
                out_iter.set( out_iter.next().processArguments( args ) );
            }
        }
        
        out.setReturnValueDescription( String.format( in.getReturnValueDescription(), args )) ;
        out.setShortDescription(       String.format( in.getShortDescription(), args ));
        out.setLongDescription(        String.format( in.getLongDescription(), args ));
        
        return out;
    }

    String interporlate( String msg ) {
        return msg.replaceAll( "<name/>",  this.getName() );
    }

    public void setNames(Symbol ... names ) {
        setNames( SchemeUtils.symbolsToStrings( names ) );
    }

    
    public Object procedureMessage() {
        String name = this.getName();
        String s = String.format( 
                   "#| (%1$s) is a documentation procedure.\n "
                 + " | Execute (help %1$s) for further information. \n"
                 + "  |# help %1$s \n",  name );
        return SchemeDocument.makeSchemeDocument( SchemeUtils.toSchemeString( s ));
    }

}

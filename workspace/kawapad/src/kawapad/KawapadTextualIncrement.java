package kawapad;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.KeyStroke;
import javax.swing.text.Caret;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import gnu.mapping.Environment;
import gnu.mapping.Procedure0;
import gnu.mapping.Procedure1;
import gnu.mapping.Procedure2;
import gnu.mapping.Values;
import kawa.standard.Scheme;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.swing.AcceleratorKeyList;
import pulsar.lib.swing.Action2;
import pulsar.lib.swing.MenuInitializer;

class KawapadTextualIncrement implements MenuInitializer {
    @Override
    public void initMenu( Map<String,JMenu> menuMap ) {
        JMenu edit = menuMap.get( "edit" );
        edit.add( TEXTUAL_INCREMENT_ACTION );
        edit.add( TEXTUAL_DECREMENT_ACTION );
    }

    
    public static void initScheme(Environment env) {
        SchemeUtils.defineVar( env, new Procedure2("add-incremental-keyword") {
            @Override
            public Object apply2(Object arg1, Object arg2) throws Throwable {
                Kawapad.getCurrent().textualIncrement.addIncrementalSymbol( 
                    SchemeUtils.anyToString( arg1 ),
                    SchemeUtils.anyToString( arg2 ));
                return Values.empty;
            }
        });
        SchemeUtils.defineVar( env, new Procedure1("delete-incremental-keyword") {
            @Override
            public Object apply1(Object arg1) throws Throwable {
                Kawapad.getCurrent().textualIncrement.deleteIncrementalSymbol( 
                    SchemeUtils.anyToString( arg1 ));
                return Values.empty;
            }
        });
        SchemeUtils.defineVar( env, new Procedure0("clear-incremental-keyword") {
            @Override
            public Object apply0() throws Throwable {
                Kawapad.getCurrent().textualIncrement.clearIncrementalSymbol();
                return Values.empty;
            }
        });
    }
    public static final String TEXTUAL_INCREMENT = "textual-increment-action";
    public static final String TEXTUAL_DECREMENT = "textual-decrement-action";
    static class IncrementalSymbol {
        final String from;
        final String to;
        final Pattern fromPattern;
        final Pattern toPattern;
        IncrementalSymbol(String from, String to) {
            super();
            if ( from == null  )
                throw new NullPointerException( "from is null" );
            if ( to == null  )
                throw new NullPointerException( "to is null");
            
            this.from = from;
            this.to = to;
            
            this.fromPattern = Pattern.compile( "\\b" + from + "\\b" );
            this.toPattern   = Pattern.compile( "\\b" + to   + "\\b" );
        }
        @Override
        public boolean equals(Object obj) {
            if ( obj instanceof KawapadTextualIncrement.IncrementalSymbol )
                return this.from.equals( ((KawapadTextualIncrement.IncrementalSymbol)obj).from );
            else
                return false;
        }
        @Override
        public int hashCode() {
            return from.hashCode();
        }
        @Override
        public String toString() {
            return this.getClass().getName() + "(" + this.from + "->" + this.to + ")";
        }
    }
    List<KawapadTextualIncrement.IncrementalSymbol> incrementalSymbols = new LinkedList<>();
    
    static final Comparator<KawapadTextualIncrement.IncrementalSymbol> COMPARATOR_FROM = new Comparator<KawapadTextualIncrement.IncrementalSymbol>() {
        @Override
        public int compare(KawapadTextualIncrement.IncrementalSymbol o1, KawapadTextualIncrement.IncrementalSymbol o2) {
            if ( o1.from.length() != o2.from.length() ) {
                return o2.from.length() - o1.from.length(); 
            } else {
                return o2.from.compareTo( o1.from );
            }
        }
    };
    static final Comparator<KawapadTextualIncrement.IncrementalSymbol> COMPARATOR_TO = new Comparator<KawapadTextualIncrement.IncrementalSymbol>() {
        @Override
        public int compare(KawapadTextualIncrement.IncrementalSymbol o1, KawapadTextualIncrement.IncrementalSymbol o2) {
            if ( o1.to.length() != o2.to.length() ) {
                return o2.to.length() - o1.to.length(); 
            } else {
                return o2.to.compareTo( o1.to );
            }
        }
    };

    void addIncrementalSymbol0( String from, String to ) {
        incrementalSymbols.add( new IncrementalSymbol( from, to ) );
    }
    void deleteIncrementalSymbol0( String from ) {
        incrementalSymbols.remove( new IncrementalSymbol( from, null ) );
    }
    void clearIncrementalSymbol0() {
        incrementalSymbols.clear();
    }

    public void addIncrementalSymbol( String from, String to ) {
        addIncrementalSymbol0( from, to );
    }
    public void deleteIncrementalSymbol( String from ) {
        deleteIncrementalSymbol0( from );
    }
    public void clearIncrementalSymbol( ) {
        clearIncrementalSymbol0();
    }
    static Pattern NUMBER_PATTERN = Pattern.compile( "[0-9\\/\\.\\-\\+]+" );

    String zeroPad( String s, int len ) {
        StringBuilder sb = new StringBuilder();
        int slen = s.length();
        for ( int i=0; i<len-slen; i++ ) {
            sb.append( '0' );
        }
        sb.append(s);
        return sb.toString();
    }
    
    String fixedFloatAdd( String s, int n, boolean doZeroPad ) {
        String wnp; // whole-number part 
        String fp;  // fraction part
        int fpLen = -1;
        {
            int tmpPos = s.indexOf( '.' );
            if ( tmpPos < 0 ) {
                wnp = s;
                fp ="";
                fpLen = 0;
            } else {
                wnp = s.substring( 0,tmpPos ).trim();
                fp =  s.substring( tmpPos+1 ).trim();
                fpLen = s.length() - tmpPos - 1;
            }
        }
        int totalLen = wnp.length() + fp.length();
        int inValue  = Integer.parseInt( wnp+fp );
        int outValue = inValue + n;
        if ( ( inValue < 0 ) && !( outValue < 0 ) ) {
            totalLen --;
        } else if ( ( 0 <= inValue ) && !( 0 <= outValue ) ) {
            totalLen ++;
        }
        
        String resultString = String.valueOf( outValue );
        if ( doZeroPad && fp.length() !=0 ) {
            resultString = zeroPad( resultString, totalLen );
            {
                int minusPos = resultString.indexOf( '-' );
                if ( 0<minusPos ) {
                    resultString = 
                            "-" + 
                                    resultString.substring( 0, minusPos ) + 
                                    resultString.substring( minusPos+1, resultString.length() );
                }
            }
        }
        
        if ( fp.length() == 0 ) {
            return resultString;
        } else {
            return  ""
                    + resultString.substring( 0, resultString.length() - fpLen ) 
                    + "." 
                    + resultString.substring(    resultString.length() - fpLen ); 
        }
        
        
    }
    String fractionAdd(String numStr, int n) {
        String[] ss = numStr.split( "\\/" );
        ss[0] = String.valueOf( fixedFloatAdd( ss[0], n, !(1<ss.length) ) ); 
        return String.join( "/", ss );
    }

    synchronized void replace(JTextComponent target, int direction ) {
        String str = target.getText();
        Caret caret = target.getCaret();
        
        int pos = caret.getDot();
        if ( str.length() < pos )
            pos = str.length();
        
        int beginPos = -1;
        {
            for ( int i=pos; 0<=i; i-- ) {
                if ( Character.isWhitespace( str.charAt( i ) ) ) {
                    beginPos = i;
                    break;
                }
            }
            if ( beginPos == -1 )
                beginPos = 0;
        }
        int endPos = -1;
        {
            Matcher m = Pattern.compile("$", Pattern.MULTILINE ).matcher( str +"\n" );
            if ( m.find( pos ) ) {
                endPos = m.start();
            }
            
            if ( endPos == -1 )
                endPos = str.length();
        }
        
        if ( endPos <= beginPos )
            return;
        
        Kawapad.logInfo( "TextualIncrementAction:" + beginPos + ":" + endPos );
        String targetStr = str.substring( beginPos , endPos );
        
        // \\b does not match the end of the string.
        // so add an extra space to the target string.
        // (Mon, 09 Sep 2019 12:00:58 +0900)
//            targetStr =  targetStr + " ";

        String foundSubstr=null;
        int foundBeginPos=Integer.MAX_VALUE;
        int foundEndPos =Integer.MAX_VALUE;

        {
            if ( foundSubstr == null ) {
                Matcher m = NUMBER_PATTERN.matcher( targetStr );
                if ( m.find() ) {
                    Scheme scheme = ((Kawapad)target).schemeSecretary.getExecutive();
                    
                    synchronized ( scheme ) {
                        try {
                            String numStr = targetStr.substring( m.start(), m.end());
                            String strResult = fractionAdd( numStr, direction );
                            
                            if ( strResult != null ) {
                                foundSubstr = strResult.toString();
                                foundBeginPos = m.start() + beginPos;
                                foundEndPos   = m.end()   + beginPos;
                            }
                        } catch (Throwable e) {
                            Kawapad.logWarn( "TextualIncrementalAddon.replace():"+e.getMessage() );
                        }
                    }
                     
                }
            }
            if ( foundSubstr == null ) {
                if ( 0 <= direction ) {
                    incrementalSymbols.sort( COMPARATOR_FROM );
                } else {
                    incrementalSymbols.sort( COMPARATOR_TO );
                }


                for ( KawapadTextualIncrement.IncrementalSymbol s : incrementalSymbols ) {
                    Pattern pattern;
                    String substr;
                    if ( 0 <= direction ) {
                        pattern = s.fromPattern;
                        substr = s.to;
                    } else {
                        pattern = s.toPattern;
                        substr = s.from;
                    }
                    
                    Matcher m = pattern.matcher( targetStr );
                    if ( m.find() ) {
                        int tempBeginPos = m.start() + beginPos;
                        int tempEndPos   = m.end()   + beginPos;
                        
                        if ( tempBeginPos < foundBeginPos ) {
                            foundBeginPos = tempBeginPos;
                            foundEndPos   = tempEndPos;
                            foundSubstr   = substr;
                        }
                        break;
                    }
                }
            }
        }
        
        Kawapad kawapad = ((Kawapad)target);
        
        if ( foundSubstr != null ) {
            kawapad.getUndoManager().startGroup();
            kawapad.getUndoManager().setSuspended( true );
            try {
                caret.setDot( foundBeginPos );
                caret.moveDot( foundEndPos );
                target.replaceSelection( foundSubstr );
                caret.setDot( foundBeginPos + foundSubstr.length() );
                caret.moveDot( foundBeginPos );
            } finally {
                kawapad.getUndoManager().setSuspended( false );
                kawapad.getUndoManager().endGroup();
            }
        }
    }

    class TextualIncrementAction extends TextAction {
        
        /** Create this object with the appropriate identifier. */
        public TextualIncrementAction() {
            super(TEXTUAL_INCREMENT);
        }

        /**
         * The operation to perform when this action is triggered.
         *
         * @param e the action event
         */
        public void actionPerformed(ActionEvent e) {
            Kawapad.logInfo("TextualIncrementAction.actionPerformed()");
            JTextComponent target = getTextComponent(e);
            if (target != null) {
                replace( target, 1);
            }
        }

        {
            putValue( Action2.CAPTION, "Increment" );
            putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_G , KeyEvent.CTRL_MASK ));
            putValue( Action.MNEMONIC_KEY , (int) 'i' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl G" );
        }
    }
    {
        addIncrementalSymbol( "foo", "bar" );
        addIncrementalSymbol( "bar", "baz" );
        addIncrementalSymbol( "baz", "foo" );
    }
    class TextualDecrementAction extends TextAction {
        /** Create this object with the appropriate identifier. */
        public TextualDecrementAction( ) {
            super(TEXTUAL_DECREMENT);
        }

        public void actionPerformed(ActionEvent e) {
            Kawapad.logInfo("TextualDecrementAction.actionPerformed()");
            JTextComponent target = getTextComponent(e);
            if (target != null) {
                replace( target, -1);
            }
        }
        {
            putValue( Action2.CAPTION, "Decrement" );
            putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D , KeyEvent.CTRL_MASK ));
            putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl D" );
        }
    }
    
    final TextualIncrementAction TEXTUAL_INCREMENT_ACTION = new TextualIncrementAction();
    final TextualDecrementAction TEXTUAL_DECREMENT_ACTION = new TextualDecrementAction();
}
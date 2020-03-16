package lamu.utils.lib;

import java.awt.Component;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.SpringLayout;

import gnu.lists.IString;
import gnu.mapping.Symbol;
import lamu.lib.log.Logger;

public class SpringLayoutUtil {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    final SpringLayout springLayout;
    final JComponent parent;
    public SpringLayoutUtil(SpringLayout springLayout, JComponent parent) {
        super();
        this.springLayout = springLayout;
        this.parent = parent;
    }

    public int o2n(Object o ) {
        int n;
        if ( o instanceof Integer ) {
            n = (Integer)o;
        } else if ( o instanceof Number ) {
            int intValue = ((Number)o).intValue();
            n = intValue ;
        } else {
            throw new RuntimeException("Unknown Type");
        }
        return n;
    }
    public Component n2c(Object o) {
        Component c;
        if ( o instanceof Component ) {
            c = (Component)o;
        } else if ( o instanceof String ) {
            if ( ((String)o).equals("p") ) {
                c = parent;
            } else {
                throw new RuntimeException("Unknown String");
            }
        } else if ( o instanceof Symbol ) {
            if ( ((Symbol)o).getLocalName().equals("p") ) {
                c = parent;
            } else {
                throw new RuntimeException("Unknown String");
            }
        } else if ( o instanceof Number ) {
            int intValue = ((Number)o).intValue();
            if ( intValue == 999 ) {
                c = parent;
            } else {
                int v = parent.getComponentCount() - 1 + intValue;
                if ( v < 0 || parent.getComponentCount() <= v )
                    c = parent;
                else
                    c = parent.getComponent(  v );
            }
        } else {
            throw new RuntimeException("Unknown Type"+ o.getClass().toString());
        }
        return c;
    }
    public String o2s(Object o) {
        String s;
        if ( o instanceof IString ) {
            s = ((IString) o).toString();
        } else if ( o instanceof String ) {
            s = (String)o;
        } else if ( o instanceof Symbol ) {
            s = ((Symbol)o).getLocalName();
        } else {
            throw new RuntimeException("Unknown Type" + o.getClass().toString() );
        }
        switch ( s ) {
            case "N" :
            case "n" :
                s="North";
                break;
            case "E" :
            case "e" :
                s="East";
                break;
            case "S" :
            case "s" :
                s="South";
                break;
            case "W" :
            case "w" :
                s="West";
                break;
            case "HE" :
            case "he" :
                s=SpringLayout.HEIGHT;
                break;
            case "WI" :
            case "wi" :
                s=SpringLayout.WIDTH;
                break;
            case "H" :
            case "h" :
                s=SpringLayout.HORIZONTAL_CENTER;
                break;
            case "V" :
            case "v" :
                s=SpringLayout.VERTICAL_CENTER;
                break;
            case "B" :
            case "b" :
                s=SpringLayout.BASELINE;
                break;
            default :
                break;
                
        }
        return s;
    }
    boolean DEBUG = false;
    public void putConstraint( Object _e1, Object _c1, Object _pad, Object _e2, Object _c2 ) {
        String e1;
        Component c1; 
        int pad;
        String e2;
        Component c2;
        e1 = o2s(_e1 );
        c1 = n2c(_c1);
        pad = o2n(_pad );
        e2 = o2s(_e2 );
        c2 = n2c(_c2);

        if ( DEBUG ) {
            logInfo( e1);
            if ( c1 instanceof JButton )
                logInfo( ((JButton)c1).getText());
            if ( c1 instanceof JPanel)
                logInfo( "Panel");
            
            logInfo( e2);
            if ( c2 instanceof JButton )
                logInfo( ((JButton)c2).getText());
            if ( c2 instanceof JPanel)
                logInfo( "Panel");
            
            logInfo( "==============");
        }
        
//      if ( e1.equals( "South") || e1.equals( "East") )
//          pad = - pad;
        
        springLayout.putConstraint( e1, c1, pad, e2, c2);
    }
}

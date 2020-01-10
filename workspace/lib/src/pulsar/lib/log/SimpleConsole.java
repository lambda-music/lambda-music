package pulsar.lib.log;

import java.awt.Component;
import java.awt.Window;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

public class SimpleConsole extends JFrame {
    public static SimpleConsole console=null;
    public static synchronized SimpleConsole getConsole() {
        if ( console == null ) {
            console = new SimpleConsole();
        }
        return console;
    }

    JTextArea textArea = new JTextArea();
    JScrollPane pane = new JScrollPane( textArea );
    {
        getContentPane().add( pane );
        textArea.setEditable( true );
        textArea.setText( "hello" );
        revalidate();
        setSize( 850,500 );
        setTitle( "Log" );
    }
    
    public static void main(String[] args) {
        SimpleConsole c = new SimpleConsole();
        c.setVisible( true );
    }

    public synchronized void addText(String s) {
        String string = "\n"+ s;
        
        Document document = this.textArea.getDocument();
        SwingUtilities.invokeLater( new Runnable() {
            @Override
            public void run() {
                try {
                    document.insertString( document.getLength() , string, null );
                } catch (BadLocationException e) {
                    e.printStackTrace();
                }
                textArea.setCaretPosition(textArea.getDocument().getLength());
                
                showConsole();
            }
        });
    }
    private void showConsole() {
        Window w = javax.swing.FocusManager.getCurrentManager().getActiveWindow();
        Component c = w.getFocusOwner();
        SimpleConsole.this.setVisible( true );
        SimpleConsole.this.toFront();
        SimpleConsole.this.repaint();
        if ( w != null ) {
            System.err.println( "*** REQUEST FOCUS ***" );
            w.toFront();
            w.setVisible( false );
            w.setVisible( true );
            if ( c != null ) {
                c.requestFocus();
            }
        } else {
            System.err.println( "*** THE ACTIVE WINDOW IS NULL ***" );
        }
        
    }
    
    public void addText( Throwable e ) {
        try (
                StringWriter sw = new StringWriter();
                PrintWriter w = new PrintWriter( sw ) )
        {
            e.printStackTrace( w );
            w.flush();
            addText( sw.getBuffer().toString() );
        } catch (IOException e1) {
            e1.printStackTrace();
        }
    }

    public void clearText() {
        SwingUtilities.invokeLater( new Runnable() {
            @Override
            public void run() {
                textArea.setText( "" );
                showConsole();
            }
        });
    }
    public void setText( String s ) {
        SwingUtilities.invokeLater( new Runnable() {
            @Override
            public void run() {
                textArea.setText( s );
                showConsole();
            }
        });
    }
}

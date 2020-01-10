package pulsar.lib.log;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
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

    public void requestClose() {
        this.dispatchEvent( new WindowEvent(this, WindowEvent.WINDOW_CLOSING ));
    }

    JTextArea textArea = new JTextArea();
    JScrollPane pane = new JScrollPane( textArea );
    JMenuBar menubar = new JMenuBar();
    JMenu fileMenu = new JMenu( "File" );
    Action closeAction = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            SimpleConsole.this.requestClose();
        }
        String caption = "Close";
        {
            putValue( Action.NAME,  caption );
            putValue( Action.MNEMONIC_KEY, (int)caption.charAt(0) );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_W, ActionEvent.CTRL_MASK) );
        }
    };
    
    {
        setJMenuBar( menubar );
        menubar.add( fileMenu );
        fileMenu.add( closeAction );
        getContentPane().add( pane );
        textArea.setEditable( true );
//        textArea.setText( "hello" );
        revalidate();
        setSize( 850,500 );
        setTitle( "Log Viewer" );
        setDefaultCloseOperation( DISPOSE_ON_CLOSE );
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

package lamu.lib.scheme;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;

import lamu.lib.log.Logger;


public class EvaluatorManager {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    Evaluator primaryEvaluator;
    public Evaluator getPrimaryEvaluator() {
        return primaryEvaluator;
    }
    public void setPrimaryEvaluator(Evaluator primaryEvaluator) {
        this.primaryEvaluator = primaryEvaluator;
    }
    Evaluator currentEvaluator;
    public Evaluator getCurrentEvaluator() {
        return currentEvaluator;
    }
    public void setCurrentEvaluator(Evaluator currentEvaluator) {
        this.currentEvaluator = currentEvaluator;
    }
    List<Evaluator> evaluatorList = new ArrayList<>();
    public List<Evaluator> getEvaluatorList() {
        return this.evaluatorList;
    }
    private List<JMenu> serverMenuList = new ArrayList<>();
    public List<JMenu> getServerMenuList() {
        return serverMenuList;
    }
    
    public void notifyUpdate() {
        for ( Iterator<JMenu> it=serverMenuList.iterator(); it.hasNext(); ) {
            JMenu serverMenu = it.next();
            if ( serverMenu != null ) {
                serverMenu.removeAll();
                int i=0;
                boolean found = false;
                for ( Evaluator evaluator : evaluatorList ) {
                    if ( evaluator == this.currentEvaluator ) {
                        found = true;
                    }
                    JMenuItem menuItem = createServerMenuItem( evaluator );
                    menuItem.setMnemonic( '0' + i );
                    serverMenu.add( menuItem );
                    i++;
                }
                if ( ! found && this.currentEvaluator != null ) {
                    JMenuItem menuItem = createServerMenuItem( this.currentEvaluator );
                    serverMenu.add( menuItem );
                }
            }
        }
    }

    private JMenuItem createServerMenuItem( Evaluator evaluator ) {
        JRadioButtonMenuItem menuItem = new JRadioButtonMenuItem( HasName.getCaption( evaluator ) );
        menuItem.addActionListener( new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                setCurrentEvaluator( evaluator );
                notifyUpdate();
            }
        });
        menuItem.setSelected( evaluator == this.currentEvaluator );
        return menuItem;
    }
    
    public EvaluatorManager( Evaluator primaryEvaluator ) {
        this.primaryEvaluator = primaryEvaluator;
        this.currentEvaluator = primaryEvaluator;
        this.evaluatorList.add( primaryEvaluator );
    }

    
    public static void initEvaluatorManager( EvaluatorManager manager, List<String> urls ) {
        ArrayList<Evaluator> list = new ArrayList<>();
        for ( String url : urls ) {
            list.add( new RemoteEvaluator( url ) );
        }
        manager.getEvaluatorList().addAll( list );
    }

    public void initialize() {
        logInfo( "EvaluatorManager.initialize()" );
        for ( Evaluator e :  this.evaluatorList ) {
            if ( e instanceof ServicingEvaluator ) {
                ((ServicingEvaluator)e).initializeEvaluator();
            }
        }
    }
    public void finalize() {
        for ( Evaluator e :  this.evaluatorList ) {
            if ( e instanceof ServicingEvaluator ) {
                ((ServicingEvaluator)e).finalizeEvaluator();
            }
        }
    }
}

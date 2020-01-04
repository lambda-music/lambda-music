package pulsar.lib.scheme;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;


public class EvaluatorList {
    Evaluator currentEvaluator;
    public Evaluator getCurrentEvaluator() {
        return currentEvaluator;
    }
    public void setCurrentEvaluator(Evaluator currentEvaluator) {
        this.currentEvaluator = currentEvaluator;
    }
    List<Evaluator> evaluatorList = new ArrayList<>();
    public Evaluator getLocalEvaluator() {
        return evaluatorList.get( 0 );
    }
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
                if ( ! found ) {
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

    /**
     * (Wed, 20 Nov 2019 11:21:28 +0900)
     * @param evaluatorList
     * @param serverMenuList
     * @return
     *          this
     */
    public EvaluatorList set( Evaluator currentEvaluator, Collection<Evaluator> evaluatorList, List<JMenu> serverMenuList ) {
        this.currentEvaluator = currentEvaluator;
        this.evaluatorList.clear();
        this.evaluatorList.addAll( evaluatorList );
        this.serverMenuList.addAll( serverMenuList );
        this.notifyUpdate();
        return this;
    }

}

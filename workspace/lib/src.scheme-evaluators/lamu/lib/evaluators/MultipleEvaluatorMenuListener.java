package lamu.lib.evaluators;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;

import lamu.lib.evaluators.MultiplexEvaluator.MultipleEvaluatorListener;

public class MultipleEvaluatorMenuListener implements MultipleEvaluatorListener {
    private List<JMenu> serverMenuList = new ArrayList<>();
    public List<JMenu> getServerMenuList() {
        return serverMenuList;
    }
    
    @Override
    public void notifyUpdate( MultiplexEvaluator multipleEvaluator ) {
        for ( Iterator<JMenu> it=serverMenuList.iterator(); it.hasNext(); ) {
            JMenu serverMenu = it.next();
            if ( serverMenu != null ) {
                serverMenu.removeAll();
                int i=0;
                boolean found = false;
                for ( Evaluator evaluator : multipleEvaluator.getEvaluatorList() ) {
                    if ( evaluator == multipleEvaluator.getPrimaryEvaluator() ) {
                        found = true;
                    }
                    JMenuItem menuItem = createServerMenuItem( multipleEvaluator, evaluator );
                    menuItem.setMnemonic( '0' + i );
                    serverMenu.add( menuItem );
                    i++;
                }
                if ( ! found && multipleEvaluator.getPrimaryEvaluator() != null ) {
                    JMenuItem menuItem = createServerMenuItem( multipleEvaluator, multipleEvaluator.getPrimaryEvaluator() );
                    serverMenu.add( menuItem );
                }
            }
        }
    }

    private JMenuItem createServerMenuItem( MultiplexEvaluator multipleEvaluator, Evaluator evaluator ) {
        JRadioButtonMenuItem menuItem = new JRadioButtonMenuItem( NameCaptionHolder.getCaption( evaluator ) );
        menuItem.addActionListener( new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                multipleEvaluator.setPrimaryEvaluator( evaluator );
            }
        });
        menuItem.setSelected( evaluator == multipleEvaluator.getPrimaryEvaluator() );
        return menuItem;
    }
}

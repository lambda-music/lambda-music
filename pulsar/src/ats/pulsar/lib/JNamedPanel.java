package ats.pulsar.lib;

import java.awt.Component;
import java.awt.LayoutManager;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;

import javax.swing.JPanel;

public class JNamedPanel extends JPanel {
	public JNamedPanel() {
		super();
	}
	public JNamedPanel(boolean isDoubleBuffered) {
		super(isDoubleBuffered);
	}
	public JNamedPanel(LayoutManager layout, boolean isDoubleBuffered) {
		super(layout, isDoubleBuffered);
	}
	public JNamedPanel(LayoutManager layout) {
		super(layout);
	}
	
	protected String nextComponentName = null;
	public void setNextComponentName( String name ) {
		this.nextComponentName = name;
	}
	public Component getComponentByName( String name ) {
		return namedMap.get( name );
	}
	protected final HashMap<String,Component> namedMap = new HashMap<>();
	@Override
	protected void addImpl(Component comp, Object constraints, int index) {
		super.addImpl(comp, constraints, index);
		if ( nextComponentName != null )
			namedMap.put(nextComponentName, comp );
		nextComponentName = null;
	}
	
	@Override
	public void remove(Component comp) {
		super.remove(comp);
		for ( Iterator<Entry<String,Component> > i = namedMap.entrySet().iterator(); i.hasNext();   ) {
			Entry<String, Component> entry = i.next();
			if ( entry.getValue() == comp )
				i.remove();
		}
		nextComponentName = null;
	}
	@Override
	public void removeAll() {
		super.removeAll();
		namedMap.clear();
		nextComponentName = null;
	}
}

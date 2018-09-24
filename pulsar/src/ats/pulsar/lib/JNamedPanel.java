package ats.pulsar.lib;

import java.awt.Component;
import java.awt.LayoutManager;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JPanel;

public class JNamedPanel extends JPanel implements JSelectableUserObject {
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
	protected final IdentityHashMap<Component,String> invNamedMap = new IdentityHashMap<>();
	@Override
	protected void addImpl(Component comp, Object constraints, int index) {
		super.addImpl(comp, constraints, index);
		if ( nextComponentName != null ) {
			namedMap.put(nextComponentName, comp );
			invNamedMap.put(comp, nextComponentName);
		} else {
			invNamedMap.put(comp, null);
		}
		nextComponentName = null;
	}
	static final Logger LOGGER = Logger.getLogger( JNamedPanel.class.getName() );
	
	private final class JNamedPanelEntry implements Map.Entry {
		private final Entry<Component, String> entry;
		private JNamedPanelEntry(Entry<Component, String> entry) {
			this.entry = entry;
		}
		@Override
		public Object getKey() {
			return entry.getValue();
		}
		@Override
		public Object getValue() {
			return entry.getKey();
		}
		@Override
		public Object setValue(Object value) {
			throw new RuntimeException( "not supported" );
		}
	}
	private final class JNamedPanelIterator implements Iterator<Map.Entry<String, Component>> {
		private final Iterator<Entry<Component, String>> iterator;
		private JNamedPanelIterator(Iterator<Entry<Component, String>> iterator) {
			this.iterator = iterator;
		}
		@Override
		public Entry<String, Component> next() {
			Entry<Component, String> entry = iterator.next();
			return new JNamedPanelEntry(entry);
		}
		@Override
		public boolean hasNext() {
			return iterator.hasNext();
		}
	}

	public Iterator< Map.Entry<String, Component>> listAllComponent() {
		Iterator<Entry<Component, String>> iterator = invNamedMap.entrySet().iterator();
		return new JNamedPanelIterator(iterator);
	}
	
	@Override
	public void remove(Component comp) {
		super.remove(comp);
		try {
			for ( Iterator<Entry<String,Component> > i = namedMap.entrySet().iterator(); i.hasNext();   ) {
				Entry<String, Component> entry = i.next();
				if ( entry.getValue() == comp )
					i.remove();
			}
		} catch ( Throwable t ) {
			LOGGER.log(Level.WARNING, "", t );
		}
		try {
			invNamedMap.remove(comp);
		} catch ( Throwable t ) {
			LOGGER.log(Level.WARNING, "", t );
		}
		nextComponentName = null;
	}
	@Override
	public void removeAll() {
		super.removeAll();
		namedMap.clear();
		invNamedMap.clear();
		nextComponentName = null;
	}
	
	@Override
	public int setSelectedByUserObject(Object userObject, boolean selected) {
		int count =0;
		for ( Component c : this.getComponents() ) {
			if ( c instanceof JUserObjectContainer ) {
				JUserObjectContainer uoc = (JUserObjectContainer)c;
				if ( userObject == null || userObject.equals( uoc.getUserObject() )) {
					uoc.setSelected( selected );
					count += 1;
				}
			}
		}
		return count;
	}
}

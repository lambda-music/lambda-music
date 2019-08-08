package pulsar.lib.swing;

import java.util.Vector;

import javax.swing.ComboBoxModel;
import javax.swing.JComboBox;

public class JPulsarComboBox<E> extends JComboBox<E> implements JSelectableUserObject {
	public JPulsarComboBox() {
		super();
	}
	public JPulsarComboBox(ComboBoxModel<E> aModel) {
		super(aModel);
	}
	public JPulsarComboBox(E[] items) {
		super(items);
	}
	public JPulsarComboBox(Vector<E> items) {
		super(items);
	}
	@Override
	public int setSelectedByUserObject(Object userObject, boolean selected ) {
		int count =0;
		if ( selected )
			for ( int i=0; i<getItemCount(); i++ ) {
				E item = getItemAt(i);
				
				if ( item instanceof PulsarListItem ) {
					PulsarListItem pitem = (PulsarListItem)item; 
					if ( userObject.equals( pitem.getUserObject() ) ) {
						this.setSelectedItem( pitem );
						count++;
						break;
					}
				}
			}
		return count;
	}
}

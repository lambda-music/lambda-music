package ats.pulsar.lib;

import java.awt.Component;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

public class Action2 {
	public static final String NAME = "CAPTION";
	public static void setCaption( Action action, String caption ) {
		action.putValue( NAME , caption );
	}
	public static <C extends AbstractButton> C processButton( C button  ) {
		Action action = button.getAction();
		if ( action != null ) {
			String caption = (String)action.getValue( NAME );
			if ( caption != null )
				button.setText( caption );
		}
		return button;
	}

	public static Component processMenuItem( Component menuItemObject ) {
		if ( menuItemObject instanceof JMenuItem ) {
			JMenuItem menuItem = (JMenuItem) menuItemObject;
			if ( menuItem instanceof AbstractButton ) {
				processButton( menuItem );
			}
			if ( menuItem instanceof JMenu ) {
				JMenu menu = (JMenu) menuItem;
				for ( Component c : menu.getMenuComponents() ) {
					processMenuItem( c );
				}
			}
		}
		return menuItemObject;
	}

	public static JMenuBar processMenuBar( JMenuBar menuBar ) {
		for ( int i=0; i<menuBar.getMenuCount(); i++ ) {
			JMenu menu = menuBar.getMenu(i);
			for ( Component c : menu.getMenuComponents() ) {
				processMenuItem( c );
			}
		}
		return menuBar;
	}
}

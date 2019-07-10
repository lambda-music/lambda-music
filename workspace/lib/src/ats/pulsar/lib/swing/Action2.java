/*
 * Action2 written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Action2 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Action2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Action2.  If not, see <https://www.gnu.org/licenses/>.
 */

package ats.pulsar.lib.swing;

import java.awt.Component;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

/**
 * This is a countermeasure for an improper designing of {@link Action#NAME }.
 * <p>
 * The {@link Action#NAME} property is treated as a caption when it is set to a
 * menu item. In the meanwhile, it is treated as its identifier which is used
 * when users need to retrieve the menu items from a list. That means you cannot
 * create two or more menu items which have a same caption string.
 * <p>
 * Use Action2#NAME property to set an independent caption string. This requires
 * additional process : calling processXXX() methods before the menu item is
 * shown. This is very ugly but I believe the cost is less than its benefit.
 * <p>
 * (Tue, 09 Jul 2019 17:53:51 +0900)
 * 
 * @author Ats Oka
 */
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

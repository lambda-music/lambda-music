package ats.pulsar.test;

import java.awt.Container;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SpringLayout;

import ats.pulsar.lib.SpringUtilities;

public class SpringTest {
	private JFrame frame;
	private SpringLayout layout;
	public SpringTest() {
		frame = new JFrame();
		layout = new SpringLayout();
		Container parent = frame.getContentPane();
		parent.setLayout( layout );
		int count =30;
		JComponent[] bs = new JComponent[ count ];
		for ( int i=0; i<count; i++ ) {
			JPanel b = new JPanel() {
				@Override
				public Dimension getPreferredSize() {
					return new Dimension(super.getPreferredSize().width , 20);
				}
				@Override
				public Dimension getMaximumSize() {
					return new Dimension(super.getMaximumSize().width , 20);
				}
				@Override
				public Dimension getMinimumSize() {
					return new Dimension(super.getMaximumSize().width , 20);
				}
			};
			b.setBorder(BorderFactory.createRaisedBevelBorder());
			

//			JButton b = new JButton("hello");
			
			bs[i] = b;
			parent.add( b );
		}
		
//		for ( int i=0; i< parent.getComponentCount(); i++ ) {
//			layout.putConstraint( SpringLayout.WEST, parent.getComponent(i), 0, SpringLayout.WEST, parent );
//		}
		SpringUtilities.makeGrid(parent, 15, 2, 0, 0, 0, 0);
		
		frame.setSize( 300, 300 );
		frame.setVisible(true);
		
	}
	public static void main(String[] args) {
		new SpringTest();
	}
}

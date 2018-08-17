package ats.pulsar.test;

import java.awt.Container;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;

import ats.pulsar.lib.SpringUtilities;

public class BoxTest {
	private JFrame frame;
	private BoxLayout layout;
	public BoxTest() {
		frame = new JFrame();
		layout = new BoxLayout( frame.getContentPane(), BoxLayout.Y_AXIS );
		Container parent = frame.getContentPane();
		parent.setLayout( layout );
		
		int count =10;
		JComponent[] bs = new JComponent[ count ];
		for ( int i=0; i<count; i++ ) {
			JPanel b = new JPanel() {
//				@Override
//				public Dimension getPreferredSize() {
//					return new Dimension(super.getPreferredSize().width , 30);
//				}
//				@Override
//				public Dimension getMaximumSize() {
//					return new Dimension(super.getMaximumSize().width , 30);
//				}
//				@Override
//				public Dimension getMinimumSize() {
//					return new Dimension(super.getMaximumSize().width , 30);
//				}
			};
			b.setPreferredSize( new  Dimension( 3000, 30));
			b.setMaximumSize(new  Dimension( 3000, 30));
			b.setMinimumSize(new  Dimension( 3000, 30));
			b.setBorder(BorderFactory.createRaisedBevelBorder());
			

//			JButton b = new JButton("hello");
			bs[i] = b;
			parent.add( b );
		}
		
		
		frame.setSize( 300, 300 );
		frame.setVisible(true);
		
	}
	public static void main(String[] args) {
		new BoxTest();
	}
}

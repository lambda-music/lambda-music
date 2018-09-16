package test;

import java.awt.Dimension;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class Test10 {
	
	static final class JPanelExtension1 extends JPanel {
		JButton b1 = new JButton("HELLO");
		{ this.add( b1 ); }
		JButton b2 = new JButton("HELLO");
		{ this.add( b2 ); }
		JButton b3 = new JButton("HELLO");
		{ this.add( b3 ); }
		JPanel p = new JPanelExtension();
		{ this.add( p ); }

		static final class JPanelExtension extends JPanel {
			JButton b1 = new JButton("HELLO");
			{ this.add( b1 ); }
			JButton b2 = new JButton("HELLO");
			{ this.add( b2 ); }
			JButton b3 = new JButton("HELLO");
			{ this.add( b3 ); }
		}
	}

	public static void main(String[] args) {
		JFrame frame = new JFrame();
		frame.setVisible(true);
		frame.setSize( new Dimension( 300,300 ));
		frame.getContentPane().add( new JPanelExtension1());
	}
}

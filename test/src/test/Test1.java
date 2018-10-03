package test;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JSlider;

public class Test1 {
	public static void main(String[] args) {
		System.out.println( System.getProperty("java.class.path").replace( ':', '\n' ) );
		JFrame frame = new JFrame();
		JMenuBar menuBar = new JMenuBar();
		frame.setJMenuBar(menuBar);
		
		JMenu m = new JMenu( "HELLO" );
		m.add( new JMenuItem("Close") );
		menuBar.add( m );
		
		Container pane = frame.getContentPane();
		JSlider slide = new JSlider();
		pane.add(slide, BorderLayout.PAGE_START );
		slide.setBorder(
				BorderFactory.createCompoundBorder(
						BorderFactory.createDashedBorder( Color.BLACK ),
						BorderFactory.createEmptyBorder(50, 50, 50, 50) 
						));
		
		
		JButton b = new JButton( "HELLLO" ) {
			@Override
			public Dimension getPreferredSize() {
				return new Dimension( 
						super.getMaximumSize().width,
						super.getPreferredSize().height
						);
			}
		};
		
		
		pane.add( b, BorderLayout.PAGE_END );
		frame.setSize(new Dimension(500, 500));
		frame.setVisible( true );
		
	}
}

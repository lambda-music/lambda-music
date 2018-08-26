package ats.pulsar.test;

import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import javax.swing.border.BevelBorder;

import ats.pulsar.lib.FlawLayout;

public class FlowInsideBoxTest {
	public static void main(String[] args) {
		JFrame frame = new JFrame();
		frame.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE );
		BoxLayout boxLayout = new BoxLayout(frame.getContentPane(), BoxLayout.PAGE_AXIS );
		frame.getContentPane().setLayout( boxLayout );
		
		JPanel panel1 = new JPanel() {
			@Override
			public Dimension getMaximumSize() {
				return new Dimension( super.getMaximumSize().width, super.getMinimumSize().height );
			}
		};
		panel1.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder(5,5,5,5), BorderFactory.createBevelBorder( BevelBorder.RAISED  ) ));
		frame.getContentPane().add( panel1 );
		panel1.setLayout(new FlawLayout() );
		panel1.add( new JButton( "HELLO1" ));
		panel1.add( new JButton( "HELLO2" ));
		panel1.add( new JButton( "HELLO3" ));
		panel1.add( FlawLayout.createNewLine() );
		panel1.add( new JButton( "HELLO3" ));
		panel1.add( new JButton( "HELLO3" ));
		panel1.add( new JButton( "HELLO3" ));
		panel1.add( new JButton( "HELLO3" ));

		JPanel panel2 = new JPanel();
		panel2.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder(5,5,5,5), BorderFactory.createBevelBorder( BevelBorder.RAISED  ) ) );
		frame.getContentPane().add( panel2 );
		
		JPanel panel3 = new JPanel();
		panel3.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder(5,5,5,5), BorderFactory.createBevelBorder( BevelBorder.RAISED  ) ) );
		frame.getContentPane().add( panel3 );
		
		
		frame.setVisible( true );
	}
}

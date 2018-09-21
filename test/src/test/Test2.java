package test;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class Test2 {
	public static void main(String[] args) {
		JFrame frame = new JFrame();
		frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
		frame.setVisible( true );
		
		frame.add( new JTestPanel() );
		frame.setSize( new Dimension(200,200 ) );

		
				
	}
	static class JTestPanel extends JPanel {
		public JTestPanel() {
			super( new BorderLayout( 5,5 ) );
		}
		JComboBox combobox = new JComboBox<>();
		{
			add( combobox );
			combobox.addItem( new JItem("HELLO") );
			combobox.addItem( new JItem("WORLD") );
			combobox.addItem( new JItem("FOO") );
		}
		class JItem {
			String message;
			public JItem( String message ) {
				this.message = message;
			}
			@Override
			public String toString() {
				return this.message;
			}
			
			
		}
	}
}

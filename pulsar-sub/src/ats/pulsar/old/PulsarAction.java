package ats.pulsar.old;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public abstract class PulsarAction implements ActionListener {
	public abstract void invoke();
	@Override
	public void actionPerformed(ActionEvent e) {
		invoke();
	}
}

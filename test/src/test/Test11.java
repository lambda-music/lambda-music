package test;

import java.util.Timer;
import java.util.TimerTask;

public class Test11 {
	public static void main(String[] args) throws InterruptedException {
		Timer timer = new Timer(true);
		timer.scheduleAtFixedRate(new TimerTask() {
			@Override
			public void run() {
				System.out.println("HELLO " + Math.floor( Math.random()*1000 ));
			}
		}, 0, 100 );
				
		Thread.sleep(5000);
		timer.cancel();
		Thread.sleep(5000);
		
	}
}

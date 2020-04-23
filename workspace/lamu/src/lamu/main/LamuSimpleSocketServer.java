package lamu.main;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;

import lamu.lib.apps.ApplicationVessel;

final class LamuSimpleSocketServer implements Runnable {
    private final ApplicationVessel owner;
    private final InputStream in;
    private final OutputStream out;
    LamuSimpleSocketServer( ApplicationVessel owner, InputStream in, OutputStream out ) {
        this.owner = owner;
        this.in = in;
        this.out = out;
    }

    @Override
    public void run() {
        try (   BufferedReader i = new BufferedReader( new InputStreamReader(  this.in  ));
                PrintStream    o = this.out instanceof PrintStream ? (PrintStream)this.out : new PrintStream( this.out ); ) 
        {
            for (;;) {
                String s = i.readLine();
                if (s == null)
                    break;
                if ("quit".equals(s) || "bye".equals(s)) {
                    o.println("ok");
                    owner.processQuit();
                    break;
                } else if ("alive?".equals(s)) {
                    o.println("yes");
                } else if ("hello".equals(s)) {
                    o.println("hello");
                } else {
                    o.println("unknown-command");
                }
            }
        } catch (IOException e) {
            LamuApplication.logError("", e);
        }
    }
}
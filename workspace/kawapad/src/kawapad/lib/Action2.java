package kawapad.lib;

public class Action2 {
    // 1.
    // Somehow pulsar.lib.swing.Action2 is invisible to kawapad-extension.
    // Its NAME field is the only dependency on for kawapad-extension.
    // Therefore, we are just copying the value to here where kawapad-extension
    // can see.
    // 2.
    // Finally I realized that kawapad-extension cannot see the classes in other projects.
    // I left this class as a reminder for the issue.
    // (Wed, 07 Aug 2019 22:45:34 +0900)
    
    public static final String NAME = quartz.lib.swing.Action2.CAPTION;
    private Action2() { }
}

package lamu.lib;

public class ConsoleChecker {
    public static boolean consoleExists() {
        return System.console() != null;
    }
}

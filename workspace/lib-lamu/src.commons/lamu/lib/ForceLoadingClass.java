package lamu.lib;

public class ForceLoadingClass {
    public static void force(Class c) {
        try {
            Class.forName(c.getName(), true, c.getClassLoader());
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

}

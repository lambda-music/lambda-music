package lamu.lib.swing;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target( FIELD )
@Retention(RetentionPolicy.RUNTIME)
public @interface AutomatedActionField {
}

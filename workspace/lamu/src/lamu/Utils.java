package lamu;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

class Utils {
    static String readAllAsString(String uri) throws IOException {
        return new String( Files.readAllBytes( Paths.get(uri)), StandardCharsets.UTF_8 );
    }
}

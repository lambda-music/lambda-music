package kawapad;

import lamu.lib.scheme.doc.DescriptiveDocumentCategory;

public class KawapadDocuments {
    public static final DescriptiveDocumentCategory DOCS = 
        DescriptiveDocumentCategory.createCategory(
            "kawapad-procedures", 
            new Runnable() {
                @Override
                public void run() {
                }
            });
}

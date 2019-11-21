package kawapad;

import java.io.File;
import java.util.HashMap;

import pulsar.lib.scheme.SchemeResult;
import pulsar.lib.scheme.scretary.SchemeSecretary;

public final class KawapadLocalEvaluator implements KawapadEvaluator, KawapadName {
    @Override
    public void evaluate( Kawapad kawapad, String text, boolean doInsertText, boolean doReplaceText, boolean doReset ) {
        kawapad.getThreadManager().startScratchPadThread( 
            new KawapadLocalEvaluatorRunnable( 
                kawapad, text, kawapad.getCurrentDirectory(), kawapad.getCurrentFile(), 
                doInsertText, doReplaceText, doReset ) );
    }
    @Override
    public String getName() {
        return "local";
    }
    
    static class KawapadLocalEvaluatorRunnable extends KawapadEvaluatorRunnable {
        File currentFile;
        File currentDirectory;
        public KawapadLocalEvaluatorRunnable(
                Kawapad kawapad, String schemeScript, File currentDirectory,
                File currentFile, boolean insertText, boolean replaceText, boolean doReset ) 
        { 
            super( kawapad, schemeScript, insertText, replaceText, true, doReset );
            this.currentDirectory = currentDirectory;
            this.currentFile = currentFile;
        }
        
        @Override
        public SchemeResult evaluate() {
            HashMap<String,Object> variables = new HashMap<>();
            kawapad.initVariables( variables );
            return SchemeSecretary.evaluateScheme( 
                kawapad.getSchemeSecretary(), 
                kawapad.getThreadInitializerList(), variables, 
                schemeScript, currentDirectory, currentFile, "scratchpad" );
        }
    }
}
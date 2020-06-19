package kawapad.lib.undomanagers;

public class UndoManagers {
    public static GroupedUndoManager create() {
// https://stackoverflow.com/questions/2547404/using-undo-and-redo-for-jtextarea
//      return new InsignificantUndoManager();
//      return new LazyGroupedUndoManager();
//      return new SimpleCompoundUndoManager();
//      return new CompoundGroupedUndoManager();
//      return new OriginalCompoundUndoManager( kawapad );
//      return new NewGroupedUndoManager();
//      return new ReimplementedUndoManager();
        
// YET ANOTHER ATTEMPT ON JUNE 2020 >>>  
//      return new JavaSLGroupedUndoManager();
//      return new EmptyGroupedUndoManager();
//      return new CompoundGroupedUndoManager();
        return new EditOnlyUndoManager();
// YET ANOTHER ATTEMPT ON JUNE 2020 <<<  

    }
    
}

 
USING Progress.Lang.*.

CLASS nl.flusso.util.TimeOutException INHERITS Progress.Lang.AppError:
   CONSTRUCTOR TimeOutException (INPUT iiCode AS INT):
      SUPER(STRING(iiCode),1).
   END CONSTRUCTOR.

   CONSTRUCTOR TimeOutException (INPUT icText AS CHARACTER):
      SUPER(icText ,1).
   END CONSTRUCTOR.

END CLASS.

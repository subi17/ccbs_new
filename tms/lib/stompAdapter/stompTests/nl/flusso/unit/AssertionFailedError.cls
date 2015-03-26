USING Progress.Lang.AppError.

CLASS nl.flusso.unit.AssertionFailedError INHERITS AppError: 
  CONSTRUCTOR PUBLIC AssertionFailedError():
    SUPER().
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC AssertionFailedError (INPUT errorMessage AS CHARACTER):
    SUPER (errorMessage, 0).
  END CONSTRUCTOR.
END CLASS.

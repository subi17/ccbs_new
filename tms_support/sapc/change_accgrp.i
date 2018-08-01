BLOCK-LEVEL ON ERROR UNDO, THROW.

FILE-INFO:FILENAME = SESSION:PARAMETER.

IF FILE-INFO:FULL-PATHNAME EQ ?
THEN DO:
   MESSAGE "File doesn't exists".
   RETURN.
END.
 
IF NOT FILE-INFO:FILE-TYPE BEGINS "F"
THEN DO:
   MESSAGE "The given file is not reqular file".
   RETURN.
END.

INPUT FROM VALUE(FILE-INFO:FILENAME) NO-ECHO.

DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO.
DEFINE VARIABLE lcLine    AS CHARACTER NO-UNDO.

REPEAT:
   
   IMPORT UNFORMATTED lcLine.

   ASSIGN   
      liCustNum = INTEGER(lcLine) NO-ERROR.
   
   IF ERROR-STATUS:ERROR
   THEN DO:
      MESSAGE lcLine + " is not integer".
      NEXT.
   END.

   FIND Customer EXCLUSIVE-LOCK WHERE
        Customer.CustNum = liCustNum NO-ERROR.
        
   IF NOT AVAILABLE Customer
   THEN DO:
      MESSAGE "Cannot find customer " + lcLine.
      NEXT.      
   END.

   IF Customer.AccGrp EQ {1}
   THEN DO:
      MESSAGE "Customer " + lcLine + " is already set to {2}".
      NEXT.      
   END.

   Customer.AccGrp = {1}.   
END.

FINALLY:
   INPUT CLOSE.
END.


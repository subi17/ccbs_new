
/*------------------------------------------------------------------------
    File        : Dump_MobSub_Status.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon March 26 11:07:37 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lSuccess AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE cFile    AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEF STREAM str-file.

{Syst/commpaa.i}

blk:
DO ON ERROR UNDO BLK, LEAVE BLK
   ON STOP UNDO BLK, LEAVE BLK:

   MESSAGE "This program will dump Mobsub.Status value for all records in Mobsub table, in a text file" SKIP(1) 
           "Do you want to continue?" 
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCont AS LOGICAL.
   IF lCont THEN 
   DO:
      
      cFile = SESSION:TEMP-DIRECTORY + "APIBSS-119-Mobsub-Status.txt".
      
      OUTPUT STREAM str-file TO VALUE(cFile).
         
         PUT STREAM str-file UNFORMATTED "MSISDN|CUSTNUM|MOBSUB.MSSEQ|MOBSUB.MSSTATUS" SKIP.
         FOR EACH mobsub WHERE mobsub.brand = Syst.Var:gcBrand NO-LOCK:
            PUT STREAM str-file UNFORMATTED mobsub.cli + "|" + STRING(mobsub.custnum) + "|" + STRING(mobsub.msseq) + "|" + STRING(mobsub.msStatus) SKIP.
         END.
         
      OUTPUT STREAM str-file CLOSE. 
      
      lSuccess = TRUE.
   END.    
END.

IF lSuccess then
      MESSAGE "Dump done successfully." SKIP 
              "See file:" cFile VIEW-AS ALERT-BOX.
ELSE 
      MESSAGE "ERROR: Dump not finished successfully" VIEW-AS ALERT-BOX.

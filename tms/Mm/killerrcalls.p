{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAMETER idaLimit AS DATE NO-UNDO.

DEFINE STREAM sDump. 

DEFINE VARIABLE liDate      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liMonth     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcDate      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMonth     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFirstPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcWholeName AS CHARACTER NO-UNDO.

DEF BUFFER newmcdr FOR MobCDR.
DEF BUFFER DelCdr  FOR ErrorCDR.

DEF TEMP-TABLE ttErrorCalls NO-UNDO
  FIELD code AS INTEGER
  FIELD sum  AS INTEGER
  INDEX code code.

IF idaLimit = ? OR idaLimit > TODAY - 30 THEN RETURN.

ASSIGN
   liDate  = DAY(TODAY) 
   liMonth = MONTH(TODAY).
       
IF liDate < 10 THEN lcDate = "0" + STRING(liDate).
ELSE                lcDate = STRING(liDate).
             
IF liMonth < 10 THEN lcMonth = "0" + STRING(liMonth).
ELSE                 lcMonth = STRING(liMonth).
       
ASSIGN
   lcFirstPart = "deleteErrCdr_" + STRING(YEAR(TODAY))
   lcName      = lcFirstPart + "_" + lcMonth + "_" + lcDate + ".log"
   lcWholeName = fCParam("DUMPOUTGOING","killerrcalls.p")
   lcWholeName = lcWholeName + lcName.

FOR EACH MobError NO-LOCK WHERE
         MobError.MobError > 0,      
    EACH MobCDR NO-LOCK WHERE
         MobCDR.ErrorCode = MobError.MobError AND
         MobCDR.DateSt    < idaLimit:

    FIND FIRST ttErrorCalls WHERE
               ttErrorCalls.code = MobCDR.ErrorCode
    EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL ttErrorCalls THEN ttErrorCalls.sum = ttErrorCalls.sum + 1.
    ELSE DO:
        CREATE ttErrorCalls.
        ASSIGN
           ttErrorCalls.code = MobCDR.ErrorCode.
           ttErrorCalls.sum  = 1.
    END.        
    
    FIND FIRST newmcdr WHERE
         RECID(newmcdr) = RECID(MobCDR) 
    EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL newmcdr THEN DO:
        IF newmcdr.ErrorCode > 0 AND
           newmcdr.DateSt    < idaLimit THEN DO:
       
          FIND FIRST McdrDtl2 WHERE
                     McdrDtl2.DateSt = NewMCDR.DateSt AND
                     McdrDtl2.DtlSeq = NewMCDR.DtlSeq EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE McdrDtl2 THEN DELETE McdrDtl2.
       
          DELETE newmcdr.
       END.   
    END.       

END.

FOR EACH ErrorCDR NO-LOCK WHERE
         ErrorCDR.DateSt < idaLimit:

    FIND FIRST ttErrorCalls WHERE
               ttErrorCalls.code = ErrorCDR.ErrorCode NO-ERROR.
    IF NOT AVAILABLE ttErrorCalls THEN DO:           
        CREATE ttErrorCalls.
        ttErrorCalls.code = ErrorCDR.ErrorCode.
    END.        
    ttErrorCalls.sum = ttErrorCalls.sum + 1.
    
    FIND FIRST DelCdr WHERE
         RECID(DelCdr) = RECID(ErrorCDR) 
    EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL DelCdr THEN DO:
       FIND FIRST ErrorDtl WHERE
                  ErrorDtl.DateSt = DelCdr.DateSt AND
                  ErrorDtl.DtlSeq = DelCdr.DtlSeq EXCLUSIVE-LOCK NO-ERROR.
       IF AVAILABLE ErrorDtl THEN DELETE ErrorDtl.
       
       DELETE DelCdr.
    END.   
END.
        
OUTPUT STREAM sDump TO VALUE(lcWholeName).

PUT STREAM sDump UNFORMATTED
   "Date"          CHR(124)
   "Error code"    CHR(124) 
   "Deleted calls" SKIP.
 
FOR EACH ttErrorCalls NO-LOCK:

    PUT STREAM sDump UNFORMATTED
       TODAY             CHR(124)
       ttErrorCalls.code CHR(124)
       ttErrorCalls.sum  SKIP.

END.

OUTPUT STREAM sDump CLOSE.

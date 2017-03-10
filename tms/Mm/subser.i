/* barrgrp.i            2004/jp
   subser validations   2008/as
*/

{Syst/commali.i}
{Func/fmakemsreq.i}

FUNCTION fSubSerSSStat RETURNS INT
(INPUT iiMsseq AS INTEGER,
INPUT icServCom AS CHAR,
INPUT iiNewSSStat AS INTEGER,
OUTPUT ocError AS CHARACTER):
  
   FIND FIRST ServCom where 
      ServCom.Brand   = gcBrand    AND 
      ServCom.ServCom = icServCom NO-LOCK NO-ERROR.
   
   /* 1 */
   IF iiNewSSStat < ServCom.SCValueRange[1]  OR
      iiNewSSStat > ServCom.SCValueRange[2] THEN DO:

      ocError = "The value must be within range " 
                + STRING(ServCom.ScValueRange[1]) 
                +  " - " 
                + STRING(ServCom.ScValueRange[2] )
                + " !".

      RETURN 1.
   END.
   
   /* 2 */
   IF INDEX(icServcom,"DELAY") > 0 THEN  DO:

      IF LOOKUP(STRING(iiNewSSStat),"5,10,15,20,25,30") = 0
      THEN DO:
         ocError = "You can only choose from 5,10,15,20,25 and 30.".
         RETURN 2.   
      END.
   END.
   
   RETURN 0.

END.

FUNCTION fSubSerSSDate RETURNS DATE
(INPUT iiMsseq AS INTEGER,
INPUT icServCom AS CHAR,
INPUT iiSSStat AS INTEGER,
INPUT idaSSDate AS DATE):

   DEFINE VARIABLE ldeActStamp AS DEC NO-UNDO. 
   DEFINE VARIABLE ldaActDate  AS DATE NO-UNDO. 
   DEFINE VARIABLE liActSec   AS DEC NO-UNDO. 
   
   ldeActStamp = fServiceActStamp(iiMsSeq,
                                 icServCom,
                                 iiSSStat).
   IF ldeActStamp > 0 THEN DO:
      fSplitTS(ldeActStamp,
               OUTPUT ldtActDate,
               OUTPUT liActSec).

      IF ldtActDate > idaSSDate OR
         (DAY(ldtActDate) = 1 AND liActSec < TIME - 120 AND
          DAY(idaSSDate) NE 1) 
      THEN RETURN ldaActDate.
      ldtActDate = ?.
      RETURN ldtActDate.
   END.

END FUNCTION. 

FUNCTION fSubSerValidate RETURNS INT
(INPUT iiMsseq AS INTEGER,
INPUT icServCom AS CHAR,
INPUT iiNewSSStat AS INTEGER,
OUTPUT ocError AS CHARACTER):

   DEF VAR liDefValue AS INT NO-UNDO.
   DEF VAR ok AS LOGICAL NO-UNDO.

   FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsseq NO-LOCK NO-ERROR.

   FIND FIRST SubSer NO-LOCK WHERE
      SubSer.MsSeq = iiMsSeq AND
      SubSer.ServCom = icServCom NO-ERROR.
   
   /* 1 */
   IF LOOKUP(Subser.ServCom,"BCG,OBA,OBO,OBR,RSA",",") > 0 THEN DO:
      RETURN 1. 
   END.
   
   /* 2 */  
   /* can service be changed from here */
   liDefValue = fServComValue(MobSub.CLIType,
                              icServCom,
                              OUTPUT ok).
   IF liDefValue = ? OR
     (liDefValue = 0 AND NOT ok) THEN DO:
      RETURN 2.
   END.

   /* 3 */
   /* Check ongoing service requests */
   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq      = iiMsSeq AND
                     MsRequest.ReqType    = 1       AND
                     MsRequest.ReqCParam1 = SubSer.ServCom AND
                     LOOKUP(STRING(MsRequest.ReqStat),"2,4,9") = 0) THEN DO:
      ocError = "There is an active change request for service." + CHR(10) + 
                "Change is not allowed before request is handled.".
      RETURN 3.
   END.

   RETURN 0. /* ok */

END FUNCTION. 

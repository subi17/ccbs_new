/* barrgrp.i            2004/jp
   subser validations   2008/as
*/

{Func/fctserval.i}

FUNCTION fSubSerSSStat RETURNS INT
(INPUT iiMsseq AS INTEGER,
INPUT icServCom AS CHAR,
INPUT iiNewSSStat AS INTEGER,
OUTPUT ocError AS CHARACTER):
  
   FIND FIRST ServCom where 
      ServCom.Brand   = Syst.Var:gcBrand    AND 
      ServCom.ServCom = icServCom NO-LOCK NO-ERROR.
  
   IF ServCom.ServCom EQ "NW" THEN DO: /* RES-885 */
      IF iiNewSSStat < 2 OR
         iiNewSSStat > 3 THEN DO:
         ocError = "The value must be within range "
                   + "2"
                   +  " - "
                   + "3"
                   + " !".
         RETURN 1.
      END.
   END.
   ELSE DO: 
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
   END.   

   RETURN 0.

END.

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


/* RES-885 NRTR */
FUNCTION fSubSerValidateNW RETURNS INT
   (INPUT iiMsseq     AS INT,
    INPUT icServCom   AS CHAR,
    INPUT icNewSSStat AS CHAR,
    OUTPUT ocError    AS CHAR):

    DEF VAR liDefValue AS INT NO-UNDO.
    DEF VAR ok AS LOGICAL NO-UNDO.

    FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsseq NO-LOCK NO-ERROR.

    FIND FIRST SubSer NO-LOCK WHERE
       SubSer.MsSeq = iiMsSeq AND
       SubSer.ServCom = icServCom NO-ERROR.

    /* 1 */
    /* "Yoigo + Orange" OR "Yoigo + Orange + Movistar" */
    IF LOOKUP(icNewSSStat,"2,3",",") = 0 THEN DO:
       ocError = "Illegal network profile.".
       RETURN 4.
    END.

    /* Profile 3 - only if customer level profile is "Yoigo + Orange + Telefonica" */
    FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsseq NO-LOCK NO-ERROR.
    IF NOT AVAIL MobSub THEN DO:
       ocError = "MobSub not found for profile.".
       RETURN 4.
    END.
       
    FIND FIRST Customer NO-LOCk WHERE
       Customer.CustNum = MobSub.CustNum.
    IF AVAIL Customer THEN DO:
       /* IF Customer.NWProfile NE "3" THEN DO:
          ocError = "Network profile not allowed.".
          RETURN 4.
       END.*/
    END.
    /* end profile 3 */

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


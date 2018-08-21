/* ----------------------------------------------------------------------
  MODULE .......: SIMCH.P
  TASK .........: Change SIM-card
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 28.11.06 
  CHANGED ......: 28.11.06     msrequest functionality
                  05.01.07 mvi dont allow new requests if there's already 
                               change request on status other than 2 (OK)
                  20.03.07 kl  fSubscriptionRequest
                  21.08.07/aam status 3 and 9 allowed in double check
                  10.11.08/as  removed unnecessary locks and cleaned code
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SIM'}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}

IF lcRight NE "RW" THEN DO:
   MESSAGE 
      "You don't have right" SKIP
      "to change SIM cards !"
   VIEW-AS ALERT-BOX.
   RETURN.   
END.

DEF INPUT PARAMETER  MsSeq LIKE MobSub.MsSeq.
                    
DEF BUFFER new-SIM      FOR SIM.
DEF BUFFER new-IMSI     FOR IMSI.
DEF BUFFER UserCustomer FOR Customer.

DEF VAR new-icc      LIKE SIM.ICC                  NO-UNDO FORMAT "X(19)".
DEF VAR SIMStat      AS i                          NO-UNDO.
DEF VAR ss-code-lost AS i                          NO-UNDO.
DEF VAR ss-code-atc  AS i                          NO-UNDO.
DEF VAR new-ss-code  AS i                          NO-UNDO.
DEF VAR new-ss-name  LIKE SIMStat.SSName          NO-UNDO.
DEF VAR UserName     AS c                          NO-UNDO.
DEF VAR ok           AS LO                         NO-UNDO FORMAT "Yes/No".
DEF VAR liReq        AS INT                        NO-UNDO.
DEF VAR ocResult     AS CHAR                       NO-UNDO.
DEF VAR lcNameSD     AS CHAR                       NO-UNDO FORMAT "X(25)".
DEF VAR liSimDeliv   AS INT                        NO-UNDO format "9".
DEF VAR lcError      AS CHAR                       NO-UNDO.
DEF VAR llCreateFees AS LOGICAL NO-UNDO.
DEF VAR ldeFee       AS DECIMAL NO-UNDO. 

form /* asks new-icc Number */
skip(1)
" NOTE: You can now change a SIM CARD for a subscription, i.e. the " skip
"       subscriber shall receive a new SIM card while his existing " skip
"       MSISDN Number remains the same."                             skip(1)
"       User ...................:" UserName format "x(36)"           skip
"       OLD SIM (ICC) ..........:" MobSub.ICC                        skip
"       NEW Status of OLD SIM ..:" SIMStat format "zzz"             
    Help "What happened to the old SIM-card? Enter status !"                 
        SIMStat.SSName format "x(24)"                               skip(1)
"       SIM Delivery ...........:" lisimdeliv 
    HELP "1= Customer Care, 2= Retailer (POS)" lcNameSD              SKIP
"       NEW SIM (ICC) ..........:" new-icc 
help "Enter new SERIAL Number of a SIM CARD (ICC)"                   SKIP(1)
 WITH  OVERLAY ROW 2 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) " CHANGE SIM FOR MSISDN " + MobSub.CLI + " "
    NO-LABELS 
    /*1 columns*/
    FRAME main.
                  
FUNCTION fCheckNewSIM RETURNS CHAR 
(icICC AS CHAR,
 iiSimDeliv AS INT):
   
   FIND new-SIM NO-LOCK WHERE 
      new-SIM.Brand = Syst.Var:gcBrand AND 
      new-SIM.ICC = icICC
   NO-ERROR.
   
   IF NOT AVAIL new-SIM THEN DO:
       RETURN "SIM with this ICC does not exist !".
   END.
   IF new-SIM.SIMStat > 2 AND 
      new-Sim.SimStat ne 9 THEN DO:
      RETURN "This SIM can not be used; status is " 
        + STRING(new-SIM.SIMStat) + " !".
   END.

   IF LOOKUP(new-SIM.Stock,"CC,RETAILER") = 0 OR
      (new-Sim.Stock  = "CC" AND iiSimdeliv ne 1 ) OR 
      (new-Sim.Stock  = "RETAILER" AND iiSimdeliv ne 2 ) THEN DO:
      RETURN "Wrong Sim stock! : " + new-Sim.Stock.
   ENd.   

   /* Get the corrsponding NEW IMSI */
   FIND FIRST new-IMSI NO-LOCK WHERE new-IMSI.ICC = new-SIM.ICC
   NO-ERROR.
   IF NOT AVAIL new-IMSI THEN DO:
      RETURN "There is NO IMSI record assigned to this SIM !".
   END.   

   RETURN "".

END FUNCTION. 
                  

FIND FIRST MSrequest WHERE 
           MsRequest.MSSeq      = MSSeq AND 
           /* two kinds of sim requests: 13 and 15 */
           (MSrequest.ReqType    = 13   OR 
            MSrequest.ReqType    = 15 ) AND
            /* accept to types: cancelled and OK */
           LOOKUP(STRING(MsRequest.ReqStatus),"2,3,4,9") = 0 
           NO-LOCK NO-ERROR.
           
IF Avail MSrequest THEN DO:
   MESSAGE 
   "ICC change request already exist #" string(msrequest.msrequest)
   VIEW-AS ALERT-BOX.
   RETURN.
END.           
           
FIND MobSub   WHERE 
     MobSub.MsSeq   = MsSeq
NO-LOCK.

FIND SIM      WHERE 
     SIM.Brand    = Syst.Var:gcBrand AND 
     SIM.ICC      = MobSub.ICC
NO-LOCK NO-ERROR.
     
IF NOT AVAIL sim THEN DO:
   MESSAGE 
   "Unknown SIM code " mobsub.icc 
   VIEW-AS ALERT-BOX.
   LEAVE.  
END.     
   
IF Mnp.MNPOutGoing:mIsMNPOutOngoing(mobsub.cli) THEN DO:
   MESSAGE "Ongoing MNP OUT request" VIEW-AS ALERT-BOX.
   LEAVE.
END.

FIND UserCustomer WHERE 
     UserCustomer.CustNum = Mobsub.CustNum
NO-LOCK NO-ERROR.
           
IF Avail UserCustomer THEN UserName = Func.Common:mDispCustName(BUFFER UserCustomer).
ELSE UserName = "".

FIND LAST MSISDN   WHERE 
     MSISDN.CLI    = MobSub.CLI  AND 
     MSISDN.ValidTo > Func.Common:mMakeTS()
NO-LOCK NO-ERROR.
IF NOT avail msisdn THEN DO:
    MESSAGE 
       "Unknown MSISDN no. " mobsub.cli SKIP(2)
       "TAKE CONTACT TECH SUPPORT"
    VIEW-AS ALERT-BOX.
    LEAVE.
END.

/*************************************************************************
*
* NOTE: This program assumes, until further, that there are NEVER more 
*       than one IMSI FOR EACH SIM !!!!!!!!
**************************************************************************/

/* search the default "lost-string" of SIMStat */
{Func/tmsparam.i SIMStatusLost return} ss-code-lost = TMSParam.IntVal.
{Func/tmsparam.i SIMStatusAtc  return} ss-code-atc  = TMSParam.IntVal.



FIND FIRST SIMStat WHERE 
           SIMStat.SIMStat = ss-code-lost
NO-LOCK NO-ERROR.
IF NOT AVAIL SIMStat THEN DO:
   MESSAGE 
   "SIM Status Code" ss-code-lost "is defined" SKIP
   "as a default for lost cards.  This code"   SKIP
   "DOES NOT EXIST !"
   VIEW-AS ALERT-BOX error TITLE
   "MISSING SIM STATUS CODE".
   RETURN.
END.   
SimStat = ss-code-lost.

FIND FIRST SIMStat WHERE 
           SIMStat.SIMStat = ss-code-atc
NO-LOCK NO-ERROR.
IF NOT AVAIL SIMStat THEN DO:
   MESSAGE 
   "SIM Status Code" ss-code-atc "is defined" SKIP
   "as a default for active cards.  This code"   SKIP
   "DOES NOT EXIST !"
   VIEW-AS ALERT-BOX error TITLE
   "MISSING SIM STATUS CODE".
   RETURN.
END.   
new-ss-code = ss-code-atc.
   
FIND SIMStat WHERE 
     SIMStat.SIMStat = ss-code-lost
NO-LOCK NO-ERROR.

PAUSE 0.
DISP 
   username
   MobSub.ICC
   lcNameSD 
   SIMStat SIMStat.SSName WHEN AVAIL SIMStat
WITH FRAME main.

MAIN:
REPEAT WITH FRAME main:

   Syst.Var:ehto = 9. 
   RUN Syst/ufkey.p.

   UPDATE
      liSimdeliv
      new-icc 
  
   WITH FRAME main EDITING:
             READKEY.
             IF LASTKEY = KEYCODE("F2") THEN NEXT.
             IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME main:
                PAUSE 0.

                IF FRAME-FIELD = "lisimdeliv" THEN DO:

                   FIND TMSCodes WHERE
                        TMSCodes.TableName = "SIM"          AND
                        TMSCodes.FieldName = "SIMCHANGE"        AND
                        TMSCodes.CodeGroup = "SIMDELTYPE"        AND
                        TMSCodes.CodeValue = STRING(INPUT lisimdeliv)
                   NO-LOCK NO-ERROR.
      
                   IF NOT AVAIL TMSCODES THEN DO:
                      BELL.
                      MESSAGE 
                      "Unknown Simdelivery TMS"
                      VIEW-AS ALERT-BOX.
                      NEXT.
                   END.
                   ASSIGN lcNameSD = TMSCodes.CodeName.
                   DISP lcNameSD WITH FRAME main. pause 0.
                END.
                IF FRAME-FIELD = "new-icc" THEN DO:
                   
                   lcError = fCheckNewSIM(INPUT INPUT new-icc, 
                                          INPUT INPUT lisimdeliv).
                   IF lcError NE "" THEN DO:
                      BELL.
                      MESSAGE lcError.
                      NEXT.
                   END.

                END.
             END.
             APPLY LASTKEY.
   END. /* EDITING */

ACTION:                            
   REPEAT WITH FRAME main:
      ASSIGN
      Syst.Var:ufk = 0 Syst.Var:ehto = 0
      Syst.Var:ufk[1] = 7 
      Syst.Var:ufk[5] = 795
      Syst.Var:ufk[8] = 8.

      IF new-icc = "" THEN Syst.Var:ufk[5] = 0.

      RUN Syst/ufkey.p.

      IF Syst.Var:toimi = 1 THEN NEXT  main.
      IF Syst.Var:toimi = 8 THEN LEAVE main.
      IF Syst.Var:toimi = 5 THEN DO:
         
         RUN Mc/charge_dialog.p(
            MobSub.MsSeq,
            (IF MobSub.PayType THEN "ICC_PREPAID" ELSE "ICC_POSTPAID"),
            OUTPUT ldeFee).
         
         llCreateFees = (ldeFee > 0).

         ok = FALSE.
         MESSAGE "Do You REALLY want to change (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.
         
         /* Double check new-icc */
         lcError = fCheckNewSIM(new-icc, lisimdeliv).
         IF lcError NE "" THEN DO:
            MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
            NEXT MAIN.
         END.

         lireq =  fSubscriptionRequest
                   (INPUT  Mobsub.MSSeq,
                    INPUT  Mobsub.Cli,
                    INPUT  Mobsub.CustNum,
                    INPUT  1,
                    INPUT  Syst.Var:katun,
                    INPUT  Func.Common:mMakeTS(),
                    INPUT  "CHANGEICC",
                    INPUT  new-icc,
                    INPUT  "",
                    INPUT  "", /*for old SIM*/
                    INPUT  "", /*for Reason info*/
                    INPUT  "", /*for ContractID*/
                    INPUT  llCreateFees,
                    INPUT  ldeFee,
                    INPUT  {&REQUEST_SOURCE_MANUAL_TMS},
                    OUTPUT ocResult).

         IF liReq = 0 THEN DO:
             MESSAGE ocresult
             view-as alert-box error.
             NEXT MAIN.
         END.
         
         IF liSimdeliv = 1 THEN DO:

            FIND MSRequest WHERE 
                 MSRequest.MSrequest = liReq
            NO-LOCK NO-ERROR.
            
            fReqStatus(19,"").

         END.
         
         /* reserve sim */
         FIND CURRENT NEW-SIM EXCLUSIVE-LOCK.        
         ASSIGN NEW-SIM.SimStat = 13. 
         RELEASE NEW-SIM.
         
         MESSAGE 
         "TMS request #" string(lireq) " has been saved to the system." SKIP(1)
         VIEW-AS ALERT-BOX TITLE "SIM CHANGED".  

         IF liSimDeliv = 1 THEN DO:
            MESSAGE
            "SIM Request has to be confirmed before final activation #"
            string(lireq)              
            VIEW-AS ALERT-BOX.
         ENd.
         
         LEAVE.
         
      END.
   END. /* Action */      

   LEAVE main.
END. /* MAIN */
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.

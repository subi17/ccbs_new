/* ------------------------------------------------------
  MODULE .......: NUMBERINQ
  FUNCTION .....: Maintain number inquiry parameters
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 07.09.05
  MODIFIED .....: 23.11.05/aam check for pending change requests
                  25.11.05/aam if public nbr -> atleast 1 channel active
                  15.12.05/aam user name from customer, not msowner      
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fsubser.i}
{Func/fctserval.i}
{Func/ffeecont.i}
{Func/service.i}
{Func/timestamp.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/fnumberinq.i}

DEF INPUT PARAMETER iiMsSeq AS INT NO-UNDO.

DEF VAR llSecret    AS LOG  NO-UNDO.
DEF VAR llDAED      AS LOG  NO-UNDO.
DEF VAR llPR        AS LOG  NO-UNDO.
DEF VAR llCD        AS LOG  NO-UNDO.
DEF VAR llSex       AS LOG  NO-UNDO.
DEF VAR llASecret   AS LOG  NO-UNDO.
DEF VAR llAFull     AS LOG  NO-UNDO.
DEF VAR lcUser      AS CHAR NO-UNDO. 
DEF VAR llCreaFee   AS LOG  NO-UNDO. 
DEF VAR liValue     AS INT  NO-UNDO.
DEF VAR lcValue     AS CHAR NO-UNDO. 
DEF VAR llFound     AS LOG  NO-UNDO.
DEF VAR liSecret    AS INT  NO-UNDO. 
DEF VAR lcAddress   AS CHAR NO-UNDO.
DEF VAR lcSex       AS CHAR NO-UNDO.
DEF VAR lcDA        AS CHAR NO-UNDO.
DEF VAR lcPR        AS CHAR NO-UNDO.
DEF VAR lcCD        AS CHAR NO-UNDO. 
DEF VAR lcOldAddr   AS CHAR NO-UNDO.
DEF VAR lcOldSex    AS CHAR NO-UNDO.
DEF VAR lcOldPub    AS CHAR NO-UNDO.
DEF VAR llPending   AS LOG  NO-UNDO.

DEF TEMP-TABLE ttParam NO-UNDO
   FIELD ParaName AS CHAR.
   
FORM
   SKIP(1)
   MobSub.CLI COLON 30
      LABEL "Subscription"
      lcUser NO-LABEL FORMAT "X(30)"
      SKIP(1)
 
   llSecret COLON 30
      LABEL "Number Inquiry (Secret Nbr)"
      HELP "Is subscription (P)ublic or (S)ecret"
      FORMAT "Secret/Public"
      SKIP(1)
      
   llASecret COLON 30
      LABEL "Address" 
      HELP "Is address (S)ecret or (P)ublic"
      FORMAT "Secret/Public"
   llAFull 
      NO-LABEL
      HELP "Is address shown (F)ully or only (P)artially"
      FORMAT "Fully/Partially"
      SKIP

   llSex COLON 30
      LABEL "Sex" 
      HELP "Is sex (P)ublic or (S)ecret i.e. only initials shown"
      FORMAT "Public/Secret"
      SKIP(1)
      
   "Can Be Published In" 
      TO 30 
      SKIP
      
   llDAED COLON 30
      LABEL "Number Inquiries (DA+ED)"
      HELP "Published in number inquiry services and services with IDs"
      FORMAT "Yes/No"
      SKIP
 
   llPR COLON 30
      LABEL "Printed Catalogues (PR)"
      HELP "Published in printed catalogues"
      FORMAT "Yes/No"
      SKIP

   llCD COLON 30
      LABEL "Electric Services (CD)"
      HELP "Published in all electric services"
      FORMAT "Yes/No"
      SKIP
        
   WITH TITLE " NUMBER INQUIRY " SIDE-LABELS
        ROW 5 CENTERED OVERLAY FRAME fInquiry.

/* convert parameter values to logical ones for ui */
FUNCTION fLogicValues RETURNS LOGIC.

   CASE lcAddress:
   WHEN "S" THEN ASSIGN llASecret = TRUE
                        llAFull   = TRUE.
   WHEN "P" THEN ASSIGN llASecret = FALSE
                        llAFull   = FALSE.
   OTHERWISE     ASSIGN llASecret = FALSE
                        llAFull   = TRUE.
   END CASE.                     

   ASSIGN llSecret  = (liSecret = 1)
          llSex     = (lcSex = "1")
          llDAED    = (lcDA  = "1")
          llPR      = (lcPR  = "1")
          llCD      = (lcCD  = "1").

END FUNCTION.


FIND MobSub WHERE 
     MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   MESSAGE "Unknown subscription" 
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND Customer WHERE
     Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Unknown customer on subscription"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

lcUser = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                          BUFFER Customer).
       
PAUSE 0.
DISP MobSub.CLI lcUser WITH FRAME fInquiry.

/* check that number inquiry is useable for this clitype */
liValue =  fServComValue(MobSub.CLIType,
                         lcNumberInq,
                         OUTPUT llAllowed).
IF liValue = ? OR NOT llAllowed THEN DO:
   MESSAGE "Number inquiry service is not in use for this CLI type"
   VIEW-AS ALERT-BOX
   INFORMATION.
   RETURN.
END.

/* for checking pending requests */
CREATE ttParam.
ttParam.ParaName = lcNumberInq.
CREATE ttParam.
ttParam.ParaName = lcSecretPar.

/* check that subscription has these service parameters */
llFound = CAN-FIND(FIRST SubSer WHERE
                         SubSer.MsSeq   = MobSub.MsSeq AND
                         SubSer.ServCom = lcNumberInq).

/* and attributes */
IF llFound THEN
FOR FIRST CTServEl NO-LOCK WHERE
          CTServEl.Brand     = gcBrand        AND
          CTServEl.ServCom   = lcNumberInq    AND
          CTServEl.CLIType   = MobSub.CLIType AND
          CTServEl.FromDate <= TODAY,
     EACH CtServAttr OF CTServEl NO-LOCK
BREAK BY CTServAttr.ServAttr:

    IF FIRST-OF(CTServAttr.ServAttr) THEN DO:
    
       CREATE ttParam.
       ttParam.ParaName = lcNumberInq + "." + CTServAttr.ServAttr.
       
       llFound = CAN-FIND (FIRST SubSerPara WHERE
                                 SubSerPara.MsSeq    = MobSub.MsSeq  AND
                                 SubSerPara.ServCom  = lcNumberInq     AND
                                 SubSerPara.ParaName = CTServAttr.ServAttr).
       IF NOT llFound THEN LEAVE.
    END.
END.

IF NOT llFound THEN DO:
   
   /* get default values from cli type if subscription hasn't got them,
      copy all missing packages at the same time */
   RUN pDefaultServices (MobSub.CLIType,
                         MobSub.MSSeq,
                         TODAY,
                         TRUE,    /* only missing */
                         FALSE,   /* create fees */
                         FALSE,  /* solog */
                         OUTPUT liValue).
END.
 
/* get current values */
liSecret = fNumberInqValues(MobSub.MsSeq,
                            OUTPUT lcDA,
                            OUTPUT lcAddress,
                            OUTPUT lcSex).

ASSIGN lcOldAddr = lcAddress
       lcOldSex  = lcSex
       lcOldPub  = lcDA
       lcPR      = SUBSTRING(lcDA,3,1)
       lcCD      = SUBSTRING(lcDA,4,1)
       lcDA      = SUBSTRING(lcDA,1,1).

fLogicValues().

/* are there pending requests concerning these parameters */
llPending = FALSE.
FOR EACH ttParam,
    EACH MsRequest NO-LOCK WHERE
         MsRequest.MsSeq      = MobSub.MsSeq     AND
         MsRequest.ReqType    = 1                AND
         MsRequest.ReqCparam1 = ttParam.ParaName AND
         MsRequest.ReqStatus  < 2:
   llPending = TRUE.
   LEAVE.
END.

lAction:
REPEAT WITH FRAME fInquiry ON ENDKEY UNDO lAction, NEXT lAction:

   PAUSE 0.
   DISPLAY llSecret
           llASecret
           llAFull
           llSex
           llDAED
           llPR
           llCD
   WITH FRAME fInquiry. 

   ASSIGN
      ufk[1]= 7  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
      ufk[5]= 15 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
      ehto = 0.
   RUN ufkey.

   if toimi = 1 THEN DO:
   
      IF llPending THEN DO:
         MESSAGE "There are pending change requests for these parameters."
         VIEW-AS ALERT-BOX TITLE " Change not allowed ".
         NEXT. 
      END.
      
      ehto = 9.
      RUN ufkey.

      lUpdate:  
      REPEAT ON ENDKEY UNDO, LEAVE:
      
          UPDATE 
          llSecret
          llASecret 
          llAFull
          llSex
          llDAED
          llPR
          llCD
          WITH FRAME fInquiry EDITING:
          
             READKEY.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME fInquiry:
             
                PAUSE 0.

                IF FRAME-FIELD = "llSecret" THEN DO:
                   /* skip attributes if service closed */
                   IF INPUT llSecret = TRUE THEN DO:
                      lcDA = fNumberInqClosed(OUTPUT lcAddress,
                                              OUTPUT lcSex).
                      ASSIGN lcPR = SUBSTRING(lcDA,3,1)
                             lcCD = SUBSTRING(lcDA,4,1)
                             lcDA = SUBSTRING(lcDA,1,1).
   
                      fLogicValues().                        

                      LEAVE lUpdate.
                   END.
                   /* get default values if opened */
                   ELSE IF INPUT llSecret = FALSE AND llSecret = TRUE 
                   THEN DO:
                      lcDA = fNumberInqOpened(Mobsub.CLIType,
                                              OUTPUT lcAddress,
                                              OUTPUT lcSex).
                      ASSIGN lcPR = SUBSTRING(lcDA,3,1)
                             lcCD = SUBSTRING(lcDA,4,1)
                             lcDA = SUBSTRING(lcDA,1,1).

                      fLogicValues().                                  
                      
                      DISPLAY llASecret
                              llAFull
                              llSex
                              llDAED
                              llPR
                              llCD 
                      WITH FRAME fInquiry.
                      
                   END.

                END.
                
                ELSE IF FRAME-FIELD = "llASecret" OR 
                        FRAME-FIELD = "llAFull"
                THEN DO:
                   IF INPUT llASecret = TRUE THEN DO:
                      llAFull = TRUE.
                      DISPLAY llAFull WITH FRAME fInquiry.
                      NEXT-PROMPT llSex.
                      NEXT.
                   END.
                END.

                ELSE IF FRAME-FIELD = "llDAED" THEN DO:
                   IF INPUT llDAED = FALSE THEN DO:
                      ASSIGN llPR = FALSE
                             llCD = FALSE.
                      DISPLAY llPR llCD WITH FRAME fInquiry.
                   END.
                END.

                ELSE IF FRAME-FIELD = "llPR" THEN DO:
                   IF INPUT llPR = TRUE THEN DO:
                      llDAED = TRUE.
                      DISPLAY llDAED WITH FRAME fInquiry.
                   END.
                   ELSE IF INPUT llPR = FALSE THEN DO:
                      llCD = FALSE.
                      DISPLAY llCD WITH FRAME fInquiry.
                   END.
                END.

                ELSE IF FRAME-FIELD = "llCD" THEN DO:
                   IF INPUT llCD = TRUE THEN DO:
                      ASSIGN llDAED = TRUE
                             llPR   = TRUE.
                      DISPLAY llDAED llPR WITH FRAME fInquiry.
                   END.
                END.

             END.
             
             APPLY LASTKEY. 
             
          END.
          
          LEAVE.
      END.

   END.
   
   ELSE IF toimi = 5 THEN DO:

      ldCurrStamp = fMakeTS().
      
      /* if number is public atleast 1st channel must be active */
      IF NOT llSecret AND NOT llDAED THEN DO:
         MESSAGE "Atleast one publishing channel must be active" 
                 "when number is public"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END. 
      
      /* has inquiry service been closed / opened */
      IF liSecret NE INTEGER(llSecret) THEN DO:

         /* make a change request */
         fServiceRequest(MobSub.MsSeq,
                         lcNumberInq,
                         INTEGER(llSecret),
                         "",
                         ldCurrStamp,
                         MobSub.Salesman,
                         TRUE,      /* fees */
                         FALSE,     /* sms */          
                         "",
                         "",
                         0,
                         FALSE,
                         OUTPUT lcValue).
      END. 
      
      /* have attributes been changed */
      
      IF llASecret THEN lcAddress = "S".
      ELSE IF llAFull THEN lcAddress = "F".
      ELSE lcAddress = "P".
      
      ASSIGN lcValue = STRING(INTEGER(llDAED),"9") + 
                       STRING(INTEGER(llDAED),"9") + 
                       STRING(INTEGER(llPR),"9") +
                       STRING(INTEGER(llCD),"9") +
                       FILL("0",4)
             lcSex   = STRING(INTEGER(llSex),"9").          
                 
      /* make requests from changes */                 
      fNumberInqAttrChanged(MobSub.MsSeq,
                            ldCurrStamp,
                            lcOldPub,
                            lcOldAddr,
                            lcOldSex,
                            lcValue,
                            lcAddress,
                            lcSex).
                            
      LEAVE lAction.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE lAction.
   END.
      
END. /* lAction */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fInquiry NO-PAUSE.


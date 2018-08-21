/* ----------------------------------------------------------------------
  MODULE .......: MSISDNCH.P
  TASK .........: Change MSISDN Numbers
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 14-07-99
  CHANGED ......: 28.11.06 jp  many issues, request functionality
                  08.01.06 mvi fixed 'new-cli-end' field check
                  20.07.07 kl  fSubscriptionRequest

  Version ......: skeleton
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MSISDN'}
{Func/fmakemsreq.i}
{Func/msisdn.i} 

IF lcRight NE "RW" THEN DO:
   MESSAGE 
      "You don't have right to" SKIP
      "change MSISDN Numbers !"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

DEFINE INPUT PARAMETER MsSeq LIKE MobSub.MsSeq.

DEFINE VARIABLE ok           AS LO                    NO-UNDO FORMAT "Yes/No".
DEFINE VARIABLE new-CLI      LIKE MSISDN.CLI          NO-UNDO.
DEFINE VARIABLE new-CLI-end  AS C                     NO-UNDO.
DEFINE VARIABLE belrange     AS C format "x(40)"      NO-UNDO EXTENT 4.
DEFINE VARIABLE lcUserName   AS C                     NO-UNDO.
DEFINE VARIABLE ms-res       AS I                     NO-UNDO.
DEFINE VARIABLE ms-use       AS I                     NO-UNDO.
DEFINE VARIABLE liReq        AS INT                   NO-UNDO.
DEFINE VARIABLE ocResult     AS CHAR                  NO-UNDO.
DEFINE VARIABLE lcPassword   AS CHAR                  NO-UNDO.
DEFINE VARIABLE lcAskPasswd  AS CHAR                  NO-UNDO.
DEFINE VARIABLE lcRestrictedPrefixes AS CHAR          NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE liStatusCode AS INT                   NO-UNDO.
DEFINE VARIABLE llCreateFees AS LOGICAL NO-UNDO. 
DEFINE VARIABLE ldeFee       AS DECIMAL NO-UNDO.


DEFINE BUFFER new-MSISDN FOR MSISDN.
DEFINE BUFFER active-MSISDN FOR MSISDN.

form /* asks MSISDN Number */
    skip(1)
    " Old MSISDN Number ....:" MobSub.CLI SKIP
    " User .................:" lcUserName format "x(35)" skip(1)
    belrange[1] AT 26                        SKIP
    belrange[2] AT 26                        SKIP
    belrange[3] AT 26                        skip(1)

    " New MSISDN number ....:" 
    new-CLI-end format "x(11)"
       HELP "Enter new MSISDN Number"

WITH
   OVERLAY ROW 2 centered COLOR VALUE(Syst.Var:cfc) NO-LABELS
   TITLE COLOR VALUE(Syst.Var:ctc) " CHANGE MSISDN Number FOR A SUBSCRIPTION "
FRAME main.

{Func/tmsparam.i MSStatusUse return}. ms-use = TMSParam.IntVal.
{Func/tmsparam.i MSStatusRes return}. ms-res = TMSParam.IntVal.

PROCEDURE pAskPassword:
   DEF OUTPUT PARAM olOk AS LOGICAL.
   
   lcPassword = fCParamC("AdminUser").
   IF lcPassword = ? THEN DO:
      MESSAGE 
         "Function currently not available. "
      VIEW-AS ALERT-BOX TITLE "INFO".
      olOk = false.
      RETURN.
   END.

   lcAskPassWd = "".
   PAUSE 0.
   UPDATE lcAskPassWd 
        BLANK
        FORMAT "X(20)" 
        LABEL "Password"
   WITH OVERLAY ROW 10 CENTERED TITLE " PASSWORD REQUIRED "
       SIDE-LABELS FRAME fPassword.
   IF lcAskPassWd NE lcPassword THEN DO:
      MESSAGE "Wrong password!" VIEW-AS ALERT-BOX.
      olOk = false.
      RETURN.
   END.
   olOk = true.
END. 

FIND FIRST MobSub WHERE
           MobSub.MsSeq = MsSeq
NO-LOCK NO-ERROR.
      
IF Mnp.MNPOutGoing:mIsMNPOutOngoing(mobsub.cli) THEN DO:
   MESSAGE "Ongoing MNP OUT request" VIEW-AS ALERT-BOX.
   LEAVE.
END.

FIND FIRST Customer WHERE
           Customer.CustNum = MobSub.CustNum
NO-LOCK NO-ERROR.
IF AVAIL Customer THEN lcUserName =
   Func.Common:mDispCustName(BUFFER Customer).


FIND FIRST MSISDN WHERE 
           MSISDN.CLI  = MobSub.CLI AND 
           MSISDN.ValidTo > Func.Common:mMakeTS()
NO-LOCK NO-ERROR.

IF NOT AVAIL msisdn THEN DO:

   MESSAGE 
      "MSISDN number is missing! " SKIP
      "Insert MSISDN number manually or "
      "take contact Tech support        "
   VIEW-AS ALERT-BOX.
   LEAVE.

END.

FIND FIRST MSRange WHERE 
           MSRange.Brand    = Syst.Var:gcBrand    AND 
           MSRange.CLIFrom <= MSISDN.CLI AND
           MSRange.CLITo   >= MSISDN.CLI
NO-LOCK NO-ERROR.

IF AVAIL MSRange THEN ASSIGN
   belrange[1] = "Belongs to MSISDN No. Range " 
   belrange[2] =  MSRange.CLIFrom + " - " +  MSRange.CLITo
   belrange[3] = "CustNo. " + string(MSRange.CustNum)
   liStatusCode = 4.
ELSE ASSIGN
   belrange[2] = "Not within any Reserved Range".
   liStatusCode = 2.

PAUSE 0.
DISP 
   MobSub.CLI  
   lcUserName
   belrange
WITH FRAME main.

MAIN:
REPEAT TRANSACTION WITH FRAME main:

   Syst.Var:ehto = 9. RUN Syst/ufkey.p.

   UPDATE
      new-CLI-end 
   WITH FRAME main EDITING:

      READKEY.

      IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME main:
                
         PAUSE 0.

         IF FRAME-FIELD = "new-CLI-end" THEN DO:

            IF INPUT FRAME main new-CLI-end = "" THEN UNDO main, LEAVE main.

            new-CLI = INPUT FRAME main new-CLI-end.
            IF new-CLI = MobSub.CLI THEN DO:
               MESSAGE
                  "Both MSISDN numbers are same !"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
               
            lcRestrictedPrefixes = fCParamC("RestrictedPreFix").
            DO i = 1 TO NUM-ENTRIES(lcRestrictedPrefixes,","):
               IF new-CLI BEGINS ENTRY(i,lcRestrictedPrefixes,",") THEN DO:
                  MESSAGE 
                     "Function not allowed for this prefix value " +
                        ENTRY(i,lcRestrictedPrefixes,",")
                  VIEW-AS ALERT-BOX ERROR.   
                  NEXT MAIN.
               END.
            END.
            
            FIND FIRST new-MSISDN WHERE  
                       new-MSISDN.Brand   = Syst.Var:gcBrand  AND
                       new-MSISDN.CLI     = new-CLI  
                       USE-INDEX CLI
            EXCLUSIVE-LOCK NO-ERROR.

            IF NOT AVAIL new-MSISDN OR
               (new-MSISDN.ValidTo <= Func.Common:mMakeTS() AND 
                  new-MSISDN.StatusCode NE 4) THEN DO:
               BELL.
               MESSAGE "MSISDN No." new-CLI "DOES NOT EXIST !".
               NEXT.
            END.
        
            ELSE DO:

               /* Check double timestamp  */ 
               FIND FIRST active-MSISDN WHERE  
                          active-MSISDN.Brand = Syst.Var:gcBrand AND
                          active-MSISDN.CLI = new-CLI  AND
                          active-MSISDN.ValidTo > Func.Common:mMakeTS() AND
                          recid(active-msisdn) ne recid(new-msisdn)
               NO-LOCK NO-ERROR.
               IF AVAIL active-MSISDN THEN DO:
                  MESSAGE 
                     "Function is not allowed. Please contact support."
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            
               IF new-MSISDN.POS = "VIP" AND 
                  getTMSRight("CCSUPER,SYST") NE "RW" THEN DO:
                  MESSAGE 
                     "Only admin users can use MSISDN from VIP stock"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               
               IF new-MSISDN.StatusCode = ms-use /* in use */ THEN DO:
                  
                  MESSAGE 
                     "Number " new-CLI " already in use by" SKIP
                     "Customer No." new-MSISDN.CustNum
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.

               END.
               
               IF new-MSISDN.StatusCode = 27 /* ongoing process */ THEN DO:
                  
                  MESSAGE 
                     "Number " new-CLI " is under process handling."
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.

               END.

               /* IF reserved TO another customer No. */
               IF new-MSISDN.StatusCode = ms-res AND
                  MobSub.CustNum NE new-MSISDN.CustNum THEN DO:
              
                  MESSAGE 
                     "This Number is Reserved to Customer" SKIP
                     "No. " new-MSISDN.CustNum SKIP
                     " - Use it anyway (Y/N) ?"
                  UPDATE ok.

                  IF NOT ok THEN NEXT.
                  RUN pAskPassword(OUTPUT ok).
                  IF NOT ok THEN NEXT.

               END.
                  
               IF new-msisdn.StatusCode = 4 THEN DO:
                
                  IF new-msisdn.validto eq 99999999.99999 THEN
                     MESSAGE
                        "This Number is quarantined"
                        "- Use it anyway (Y/N) ?"
                     UPDATE ok.
                  ELSE
                     MESSAGE
                        "This Number is quarantined until"
                        ENTRY(1,Func.Common:mTS2HMS(new-msisdn.validto)," ")
                        "- Use it anyway (Y/N) ?"
                     UPDATE ok.
                  
                  IF NOT ok THEN NEXT.
                  RUN pAskPassword(OUTPUT ok).
                  IF NOT ok THEN NEXT.
        
               END.

               IF new-MSISDN.ValidFrom < Func.Common:mMakeTS() AND 
                  new-msisdn.ValidTo > Func.Common:mMakeTS() THEN DO:

                  IF new-msisdn.StatusCode = 0 THEN DO:
              
                     MESSAGE
                        "This Number is marked not open to market"
                        " - Use it anyway (Y/N) ?"
                     UPDATE ok.
     
                     IF NOT ok THEN NEXT.
            
                  END.
           
               END.

               IF LOOKUP(STRING(new-msisdn.statuscode),"0,1,2,4") = 0 THEN DO:
                  MESSAGE 
                    "Cannot change to msisdn with status" new-msisdn.statuscode
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

            END.
     
         END.
             
      END.
         
      APPLY LASTKEY.

   END. /* EDITING */

   ACTION:
   REPEAT WITH FRAME main:

      ASSIGN
         Syst.Var:ufk    = 0
         Syst.Var:ehto   = 0
         Syst.Var:ufk[1] = 7 
         Syst.Var:ufk[5] = 261
         Syst.Var:ufk[8] = 8.
      
      RUN Syst/ufkey.p.

      IF Syst.Var:toimi = 1 THEN NEXT  main.

      IF Syst.Var:toimi = 8 THEN LEAVE main.

      IF Syst.Var:toimi = 5 THEN DO:
         
         RUN Mc/charge_dialog.p(
            MobSub.MsSeq,
            (IF MobSub.PayType THEN "MSISDN_PREPAID" ELSE "MSISDN_POSTPAID"),
            OUTPUT ldeFee).
         
         llCreateFees = (ldeFee > 0).

         ok = FALSE.
         MESSAGE "Do You REALLY want to change (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.
         fMakeMsidnHistory(INPUT RECID(new-MSISDN)).
 
         ASSIGN 
            MSISDN.CustNum    = MobSub.CustNum
            MSISDN.StatusCode = 27.

         lireq =  fSubscriptionRequest
                   (INPUT  Mobsub.MSSeq,
                    INPUT  Mobsub.Cli,
                    INPUT  Mobsub.CustNum,
                    INPUT  liStatusCode,
                    INPUT  Syst.Var:katun,
                    INPUT  Func.Common:mMakeTS(),
                    INPUT  "CHANGEMSISDN",
                    INPUT  new-cli,
                    INPUT  "",
                    INPUT  "", /*for old SIM*/
                    INPUT  "", /*for Reason info*/
                    INPUT  "", /*for ContractID*/
                    INPUT  llCreateFees,
                    INPUT  ldeFee,
                    INPUT  {&REQUEST_SOURCE_MANUAL_TMS},
                    OUTPUT ocResult).

         MESSAGE 
            "TMS request #" string(lireq) " has been saved to the system." SKIP(1)
         VIEW-AS ALERT-BOX TITLE "MSISDN CHANGE REQUEST".  
         
         LEAVE Action.

      END.

   END. /* Action */      

   LEAVE main.

END. /* MAIN */

HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.

/* -----------------------------------------------
  MODULE .......: viewpres.p 
  FUNCTION .....: View a preselction TRANSACTION record
  SOLUTION .....: TMS
  CREATED ......: 18.11.99 pt
  MODIFIED .....: 26.06.00 pt more fields displayed
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT PARAMETER CLI LIKE Presel.CLI NO-UNDO.
/*
FIND FIRST Presel where CustNum = 1001. CLI = Presel.CLI. 
*/ 

DEF VAR pstypes     AS C  NO-UNDO.
DEF VAR PsType      AS C  NO-UNDO.
DEF VAR dpstype     LIKE Presel.PsType NO-UNDO.
DEF VAR pecode      AS C  NO-UNDO.
DEF VAR pename      LIKE PreselErr.PSEName.



pstypes = "NONE,NATIONAL,INTERNATIONAL,NAT & INT".

FORM
   Presel.CustNum   label "Cust No ......" Customer.CustName NO-LABEL SKIP
   Presel.PsType   label "PreSelectType." 
   PsType FORMAT "X(30)" NO-LABEL          SKIP
   /*Presel.ReturnCode*/
   pecode          label "Status ......." pename NO-LABEL   SKIP
   Presel.ErrText   label "Error expl. .."                   SKIP(1)

   "AUTHORISATION DATA:" SKIP
   Presel.AuthDate label "AuthDate ....."                          
   Presel.AuthNo   label "AuthorNo ....."  AT 30                   SKIP
   Presel.Orderer  label "Orderer ......"                          SKIP(1)

   "EXPORT DATA:" SKIP
   Presel.SentDate label "Send Date ...."
   Presel.FileSeq1 label "OutFSeq ......"  AT 30                   SKIp(1)
   "CONFIRMATION DATA:" SKIP
   Presel.ConfDate label "ConfirmDate .."                        
   Presel.FileSeq2 label "InFSeq ......."  AT 30
   Presel.AuthNo2  label "AuthorNo 2 ..."  AT 30                   SKIP

WITH
   OVERLAY CENTERED ROW 3 SIDE-LABELS 
   TITLE " CARRIER PRESELECTION RECORD FOR CLI " + CLI + " "
   FRAME Presel.


FIND Presel WHERE Presel.CLI = CLI NO-LOCK NO-ERROR.

IF NOT AVAIL Presel THEN DO:
  MESSAGE 
  " This CLI has no Carrier Preselection Definition !"
  VIEW-AS ALERT-BOX INFORMATION.
  LEAVE.
END.


IF Presel.FileSeq2 =  0 THEN DO:
   if Presel.SentDate = ? THEN pename = "NEW RECORD, NOT Sent TO TELIA".
                          ELSE pename = "NEW RECORD, Sent TO TELIA".
END.                         
ELSE DO: 
   IF Presel.ReturnCode = 0 THEN pename =  "SUCCESFULLY REGISTERED BY TELIA".
   ELSE DO:
      FIND PreselErr WHERE PreselErr.PSError = Presel.ReturnCode.
      pecode = STRING(Presel.ReturnCode).
      IF AVAIL PreselErr THEN  pename = PreselErr.PSEName.
   END.   
END.  

FIND Customer OF Presel NO-LOCK.

PsType = ENTRY(Presel.PsType + 1,pstypes).

   PAUSE 0.
   DISP 

   Presel.CustNum   Customer.CustName
   Presel.Orderer
   Presel.PsType
   PsType 
   pecode          pename 
   Presel.ErrText
   Presel.AuthNo
   Presel.AuthNo2
   Presel.AuthDate
   Presel.SentDate
   Presel.ConfDate
   Presel.FileSeq1
   Presel.FileSeq2
WITH 
   FRAME Presel.


   ASSIGN
   ufk = 0 ufk[8] = 8 ehto = 0. RUN Syst/ufkey.

   HIDE FRAME Presel NO-PAUSE.

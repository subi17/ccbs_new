/* -------------------------------------------------------------------------
  MODULE .......: MSISDNiv.p
  FUNCTION .....: Mobile Subscriber ISDN number invoice VALUE                 
  APPLICATION ..: NN & TN
  CREATED ......: 04.04.2000 jp
  MODIFIED .....: 27.09.2001 jp use InvSeq
                  13.03.2003 tk tokens
                  04.04.2003 tk ttCall.Pricelist no longer exist =>
                                removed vat handling
                  28.05.2003 tk numbers were 100 times too big              
                  10.09.03   jp Brand
  Version ......: M15
  ------------------------------------------------------------------------ */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'mobsub'}
{callquery.i}

DEF INPUT PARAMETER  iiInvSeq AS INT    NO-UNDO.
DEF INPUT PARAMETER  CLI      AS CHAR   NO-UNDO.
DEF INPUT PARAMETER  lcCrit   AS CHAR   NO-UNDO.

DEF VAR   changes            AS DEC    NO-UNDO.
DEF VAR   changes%           AS DEC    NO-UNDO.   

DEF VAR alvpros   AS DE NO-UNDO init 22.
DEF VAR lkm1      AS i  NO-UNDO.
DEF VAR lkm2      AS i  NO-UNDO.
def var summa1    as de no-undo format "-z,zzz,zzz.99".
def var summa2    as de no-undo .
def var summa1a   as de no-undo format "-z,zzz,zzz.99".
def var summa2a   as de no-undo format "-z,zzz,zzz.99".
def var pvm1      as da no-undo format "99.99.99".
def var pvm2      as da no-undo format "99.99.99".
DEF VAR lccli     AS C  NO-UNDO.
DEF VAR ldePrevDate AS DEC NO-UNDO.

DEF VAR ldaFrom AS DATE NO-UNDO.
DEF VAR ldaTo   AS DATE NO-UNDO.

DEF TEMP-TABLE BufferCall  NO-UNDO
FIELD cli       AS C    
FIELD datest    AS DATE 
FIELD ccn       AS INT 
FIELD billCode  AS CHAR 
FIELD Price     AS DEC 
INDEX cli is primary unique cli datest billcode ccn
INDEX datest    cli datest 
INDEX billcode  cli billcode
INDEX ccn       cli ccn.

DEF  TEMP-TABLE ttCall NO-UNDO LIKE  Mobcdr
   FIELD CDRTable AS CHAR 
   INDEX date     Datest TimeStart
   INDEX CustNum  CustNum Datest TimeStart
   INDEX Cli      Cli     Datest TimeStart gsmbnr spocmt.
               


FORM 
  BufferCall.datest COLUMN-LABEL "Date" 
  summa2        COLUMN-LABEL "Total price" FORMAT ">>>,>>9.99"
  

WITH ROW 5 CENTERED width 40 OVERLAY 10  DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " Calls by " + lcCrit + " "  
    + string(pvm,"99-99-99") 
FRAME aa.

FORM 
  BufferCall.billcode COLUMN-LABEL "BillCode" 
  BillItem.Biname format "X(35)"   COLUMN-LABEL "Billing Item Name" 
  summa2          COLUMN-LABEL "Total price" FORMAT ">>>,>>9.99"

WITH ROW 5 CENTERED width 60 OVERLAY 10  DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " Calls by " + lcCrit + " " 
    + string(pvm,"99-99-99") 
FRAME ab.

FORM 
  BufferCall.ccn COLUMN-LABEL "CCN" 
  ccn.ccnname  format "X(35)"   COLUMN-LABEL "Report destination" 
  summa2          COLUMN-LABEL "Total price" FORMAT ">>>,>>9.99"

WITH ROW 3 CENTERED width 60 OVERLAY 13  DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " Calls by " + lcCrit + " " 
    + string(pvm,"99-99-99") 
FRAME ac.

IF iiInvSeq > 0 THEN DO:
   FIND FIRST InvSeq where
              InvSeq.Invseq = iiInvSeq NO-LOCK NO-ERROR.
           
   IF NOT AVAIL INVSEQ THEN DO:
      MESSAGE
      "Unknown Invoice sequence nbr:" + string(iiInvseq)
      VIEW-AS ALERT-BOX.
      LEAVE.
   END.

   ASSIGN
      ldaFrom = InvSeq.FromDate
      ldaTo   = InvSeq.ToDate.
END.
ELSE ASSIGN
   ldaFrom = 1/1/06
   ldaTo   = TODAY.

DEFINE VARIABLE tthCDR     AS HANDLE    NO-UNDO.

DEF var lierrorcodeout as i no-undo.

tthCDR = TEMP-TABLE ttCall:HANDLE.


fMobCDRCollect(INPUT "post,pre",
               INPUT gcBrand,
               INPUT katun,
               INPUT ldaFrom,
               INPUT ldaTo,
               INPUT 0,
               INPUT "",
               INPUT CLI,
               INPUT iiInvSeq,
               INPUT 0,
               INPUT "",
               INPUT "",
               INPUT "",
               INPUT 0,
               INPUT-OUTPUT liErrorCodeOut,
               INPUT-OUTPUT tthCDR).
                       

FOR EACH ttCall NO-LOCK WHERE
         ttCall.CLI  = CLI,
   FIRST InvSeq NO-LOCK WHERE 
         InvSeq.InvSeq = ttCall.InvSeq AND
         InvSeq.Billed = FALSE:

   FIND FIRST BufferCall WHERE 
              BufferCall.cli      = ttCall.cli       AND 
              BufferCall.datest   = ttCall.datest    AND 
              BufferCall.BillCode = ttCall.billcode  AND 
              BufferCall.ccn      = ttCall.ccn no-error.
                    
   IF not avail BufferCall then do:
      create BufferCall.        
      ASSIGN
      BufferCall.cli      = ttCall.cli
      BufferCall.datest   = ttCall.datest
      BufferCall.BillCode = ttCall.billcode
      BufferCall.ccn      = ttCall.ccn.
   END.
   ASSIGN
      BufferCall.price    = BufferCall.Price + ttCall.amount.


END.

PAUSE 0.

   
IF not can-find (first BufferCall) THEN DO:
   MESSAGE
   "Cant find any unbilled calls for" cli
   VIEW-AS ALERT-BOX.
   LEAVE.
ENd.
   
IF lcCrit = "DATE" THEN DO:
   FOR EACH BufferCall use-index date 
   BREAK
   BY BufferCall.datest.

      summa2 = summa2 + BufferCall.price.
      
      IF LAST-OF(BufferCall.datest) THEN DO:
            disp BufferCall.datest summa2 
            WITH FRAME aa . down with frame aa.
            ASSIGN
               ldeprevdate = summa2
               summa2      = 0.
      END.
  END.
END.
   
ELSE IF lcCrit = "PRODUCT" THEN DO:
   FOR EACH BufferCall use-index billcode
   BREAK
   BY BufferCall.BillCode.

      summa2 = summa2 + BufferCall.price.
      
      IF LAST-OF(BufferCall.billcode) THEN DO:
         FIND billitem where 
              billitem.brand    = gcBrand         AND 
              billitem.billcode = BufferCall.billcode NO-LOCK NO-ERROR.
              
         disp BufferCall.billcode 
              BillItem.BIName 
              summa2  WITH FRAME ab . down WITH FRAME ab.
         summa2 = 0.
      ENd.
   END.
END.

ELSE IF lcCrit = "CCN" THEN DO:
   FOR EACH BufferCall use-index ccn
   BREAK
   BY BufferCall.ccn.

      summa2 = summa2 + BufferCall.price.
      
      IF LAST-OF(BufferCall.ccn) THEN DO:
         FIND ccn where 
              ccn.brand  = gcBrand         AND 
              ccn.ccn    = BufferCall.ccn NO-LOCK NO-ERROR.
              
         disp BufferCall.ccn 
              ccn.ccnname 
              summa2  WITH FRAME ac . down WITH FRAME ac.
         summa2 = 0.
      ENd.
   END.
END.



message "Press ENTER to continue !".
PAUSE no-message.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.



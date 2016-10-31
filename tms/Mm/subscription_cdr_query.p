/* ----------------------------------------------------------------------
  MODULE .......: subscription_cdr_query.p
  TASK .........: Browse all subscription's cdrs (including errorneus)
  AUTHOR .......: aam
  CREATED ......: 21.06.10
  ---------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'MobCDR'} 

DEF VAR lcCLI        AS CHAR NO-UNDO.
DEF VAR liErrorCode  AS INT  NO-UNDO.
DEF VAR lcReasonCode AS CHAR NO-UNDO.
DEF VAR ldaFromDate  AS DATE NO-UNDO.
DEF VAR ldaToDate    AS DATE NO-UNDO.
DEF VAR llAccept     AS LOG  NO-UNDO.
DEF VAR llErrorCodes AS LOG  NO-UNDO.

FORM
   SKIP(1)
   lcCLI AT 2 
      LABEL "MSISDN/FIXED NUMBER"  
      HELP "MSISDN/FIXED NUMBER"
      FORMAT "X(12)" 
      SKIP
   llErrorCodes AT 2
      LABEL "Error Codes" 
      HELP "Collect also errorneus CDRs (yes) or only succesfully rated (no)"
      FORMAT "Yes/No"
   SKIP(1)
   WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " CDR QUERY " FRAME fCLI.
       
PAUSE 0.
VIEW FRAME fCLI.

REPEAT WITH FRAME fCLI ON ENDKEY UNDO, LEAVE:
   ehto = 9.
   RUN ufkey.p.

   PAUSE 0.
   UPDATE lcCLI llErrorCodes WITH FRAME fCLI.

   IF lcCLI > "" AND 
    ( lcCLI BEGINS "8" OR
      lcCLI BEGINS "9" ) THEN DO:
      IF NOT CAN-FIND(FIRST MsOwner WHERE MsOwner.FixedNumber EQ lcCLI) THEN DO:
         MESSAGE "Unknown Fixed Number"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
   END.
   ELSE IF lcCLI > "" AND
      NOT CAN-FIND(FIRST MSOwner WHERE MSOwner.CLI = lcCLI) THEN DO:
      MESSAGE "Unknown MSISDN" 
      VIEW-AS ALERT-BOX ERROR.
      NEXT.
   END.

   LEAVE.
END.

HIDE FRAME fCLI NO-PAUSE.

IF lcCLI = "" OR KEYLABEL(LASTKEY) = "F4" THEN RETURN.
      
RUN mobguard2.p(INPUT  TRUE,
                OUTPUT lcReasonCode,
                OUTPUT ldaFromDate,
                OUTPUT ldaToDate,
                OUTPUT llAccept).

IF NOT llAccept THEN RETURN.

RUN mobcallbr.p(INPUT "post,pre",
                INPUT  ldaFromDate,
                INPUT  ldaToDate,
                INPUT  0,
                INPUT  "",
                INPUT  lcCLI,
                INPUT  0,
                INPUT  0,
                INPUT  "",
                INPUT  "",
                INPUT  lcReasonCode,
                INPUT  IF llErrorCodes THEN ? ELSE 0,
                INPUT  0).
                                

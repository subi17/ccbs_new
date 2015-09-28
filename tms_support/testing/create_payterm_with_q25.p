{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{fmakemsreq.i}

DEF VAR lccli AS CHAR NO-UNDO format "x(10)".
DEF VAR lcResult AS CHAR NO-UNDO.
DEF VAR liRequest AS INT NO-UNDO.
DEF VAR ldaActivationDate AS DATE NO-UNDO format "99-99-9999".
DEF VAR lcPayterm AS CHAR NO-UNDO INIT "PAYTERM24_3".
DEF VAR ldeResidualFee AS DEC NO-UNDO init 50.
DEF VAR ufkey AS LOG NO-UNDO INIT TRUE.

assign
   ldaActivationDate = today.

FORM
    "MSISDN .....:" lcCli FORMAT "x(10)" SKIP
    "Act.Day ....:" ldaActivationDate SKIP
    "Installment.:" lcPayterm FORMAT "x(20)" SKIP
    "ResidualFee.:" ldeResidualFee FORMAT ">>9.99" SKIP
WITH OVERLAY ROW 4 centered
    TITLE " Create installment contract with Q25 "
    NO-LABELS
    FRAME lis.

RUN pUserInput.
/* IF  User Wanted TO Cancel this Change TRANSACTION */
IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
KEYLABEL(lastkey) = "F4" THEN UNDO, RETURN.

PROCEDURE pUserInput:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      DISP lcCLI
           lcPayterm
           ldaActivationDate
           ldeResidualFee WITH FRAME lis.

      UPDATE
         lccli skip
         ldaActivationDate  skip
         lcPayterm  skip
         ldeResidualFee skip
      WITH FRAME lis EDITING:

         IF ufkey THEN DO:
            ASSIGN ehto = 9. RUN ufkey.p.
            ufkey = false.
         END.

         READKEY.

         nap = keylabel(lastkey).


         IF LOOKUP(nap,poisnap) > 0 THEN DO:

            IF FRAME-FIELD = "lcCli" THEN DO:
               FIND FIRST mobsub WHERE
                          mobsub.cli = INPUT lcCli
               NO-LOCK NO-ERROR.
               IF NOT AVAIL mobsub then do:
                  MESSAGE "Subcription" input lcCli "not found"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               end.
            END.

            IF FRAME-FIELD = "lcPayterm" THEN DO:
               FIND FIRST DayCampaign WHERE
                          DayCampaign.Brand = gcBrand and
                          DayCampaign.DcEvent begins "PAYTERM" and
                          DayCampaign.DcEvent eq INPUT lcPayterm
               NO-LOCK NO-ERROR.
               IF NOT AVAIL DayCampaign then do:
                  MESSAGE "Uknown installment contract" input lcPayterm
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               end.
            END.

         END.

         APPLY LASTKEY.

      END.
      LEAVE.
   END.
END.

if lcPayterm eq "" or
   not avail mobsub or
   ldaActivationDate eq ? then do:

   MESSAGE "Incorrect parameters" VIEW-AS ALERT-BOX.
   return.

end.


liRequest = fPCActionRequest(mobsub.msseq, /* subscription id */
                             lcPayterm,
                             "act",
                             fMake2Dt(ldaActivationDate,0), /* activation_ts */
                             true,
                             "5",
                             "",
                             0,
                             FALSE,
                             "",
                             55, /* residual (q25) fee */
                             0,
                             OUTPUT lcResult).

IF liRequest eq 0 then MESSAGE "ERROR:" lcResult VIEW-AS ALERT-BOX.
ELSE MESSAGE "OK, request id" liRequest VIEW-AS ALERT-BOX.

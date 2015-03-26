{commpaa.i}
katun = "anttis".
gcBrand = "1".
{date.i}

find daycampaign where
     daycampaign.dcevent = "SMSFREEOCT10" no-lock.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
input from as_yot945.input2.

def stream slog.
output stream slog to as_yot945.log2 append.

def var liContractID as int no-undo.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

repeat:

etime(true).

DEFINE VARIABLE ldeBegin AS DECIMAL NO-UNDO. 
ldeBegin = fMakeTS().

do trans:

   import unformatted lcLine.
   
   i = i + 1.
   if i <= 3 then next. 

 /*  if i > 3 then leave.  */

   find mobsub where
        mobsub.cli = lcLine NO-LOCK no-error.
   IF NOT AVAIL mobsub then do:
      put stream slog unformatted lcLine "|ERROR not found" skip.
      next.
   end.
 
   IF index(mobsub.clitype, "cont") = 0 then do:
      put stream slog unformatted lcLine "|ERROR subs. type is " mobsub.clitype skip.
      next.
   end.

  find first dccli where
   dccli.msseq = mobsub.msseq and
   dccli.dcevent = daycampaign.dcevent NO-LOCK no-error.
   IF AVAIL dccli then do:
      put stream slog unformatted lcLine "|ERROR contract exists" skip.
      next.
   end.

  liContractID = NEXT-VALUE(PerContractID).
  
   CREATE DCCLI.

   REPEAT:
      DCCLI.PerContractID = liContractID NO-ERROR.

      VALIDATE DCCLI NO-ERROR.

      IF ERROR-STATUS:ERROR OR DCCLI.PerContractID = 0 THEN DO:
         liContractID = NEXT-VALUE(PerContractID).
         NEXT.
      END.
      ELSE LEAVE.
   END.

   ASSIGN DCCLI.Brand        = gcBrand
          DCCLI.DCEvent      = daycampaign.dcevent
          DCCLI.MsSeq        = mobsub.msseq
          DCCLI.CLI          = mobsub.cli
          DCCLI.ContractDate = 10/1/2010
          DCCLI.ValidFrom    = 10/1/2010
          DCCLI.ValidTo      = 10/31/2010
          DCCLI.CreateFees   = ( LOOKUP(DayCampaign.DCType,"3,5") > 0 AND
                                DayCampaign.TermFeeCalc > 0).
   
   run cli_rate.p (mobsub.cli,
                   10/1/10,
                   10/31/10,
                   true). 
  
   put stream slog unformatted dccli.cli "|" dccli.msseq "|" dccli.percontractid skip. 
end. /* trans */
end.

DEFINE VARIABLE ldeEnd AS DECIMAL NO-UNDO. 
ldeEnd = fMakeTS().


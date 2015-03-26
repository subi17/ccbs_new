/* ------------------------------------------------------
  MODULE .......: clclicol.p
  FUNCTION .....: transfer CLIs to credit loss customer 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 04.12.02
  MODIFIED .....: 15.01.03/aam set clstamp to 23:59:59 instead of 24:00:00,
                               check billed-state for calls
  Version ......: Cubio
  ------------------------------------------------------ */

{commali.i}
{timestamp.i}

DEF INPUT  PARAMETER iiFromCust AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiToCust   AS INT  NO-UNDO. 
DEF INPUT  PARAMETER icCLI      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idtDate1   AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtDate2   AS DATE NO-UNDO. 
DEF OUTPUT PARAMETER oiCount    AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER odtFound1  AS DATE NO-UNDO.
DEF OUTPUT PARAMETER odtFound2  AS DATE NO-UNDO. 

DEF VAR ldFromStamp AS DEC NO-UNDO.
DEF VAR ldToStamp   AS DEC NO-UNDO. 


DEF TEMP-TABLE ttCLI NO-UNDO
   FIELD CLI      AS CHAR
   FIELD FromDate AS DATE
   FIELD ToDate   AS DATE
   INDEX CLI CLI. 


/* collect CLIs */
FOR EACH FixCDR NO-LOCK WHERE
         FixCDR.InvCust = iiFromCust AND
         FixCDR.Date >= idtDate1   AND
         FixCDR.Date <= idtDate2,
   /* check also custnum and billed-state, eventhough unknown-customer
      should never be billed -> rerate should be able to handle all calls
      that get through this check */
   FIRST InvSeq NO-LOCK WHERE
         InvSeq.InvSeq  = FixCDR.InvSeq  AND
         InvSeq.CustNum = FixCDR.InvCust AND
         InvSeq.Billed  = FALSE:
    
   IF icCLI NE "" AND
      FixCDR.CLI NE icCLI
   THEN NEXT. 
   
   FIND FIRST ttCLI WHERE
      ttCLI.CLI = FixCDR.CLI NO-ERROR.
   IF NOT AVAILABLE ttCLI THEN DO:
      CREATE ttCLI.
      ASSIGN ttCLI.CLI      = FixCDR.CLI
             ttCLI.FromDate = FixCDR.Date
             ttCLI.ToDate   = FixCDR.Date.
   END.

   ASSIGN ttCLI.FromDate = MIN(ttCLI.FromDate,FixCDR.Date)
          ttCLI.ToDate   = MAX(ttCLI.ToDate,  FixCDR.Date). 

END.           
   
/* set default values */
ASSIGN odtFound1 = idtDate2
       odtFound2 = idtDate1.
   
/* transfer CLIs to credit loss customer */
FOR EACH ttCLI:

   ASSIGN ldFromStamp = fHMS2TS(ttCLI.FromDate,"0")
          ldToStamp   = fHMS2TS(ttCLI.ToDate,"23:59:59")
          odtFound1   = MIN(odtFound1,ttCLI.FromDate)
          odtFound2   = MAX(odtFound2,ttCLI.ToDate). 
          
   FIND FIRST CLI EXCLUSIVE-LOCK WHERE
      CLI.CustNum = iiToCust AND
      CLI.CLI     = ttCLI.CLI NO-ERROR. 
      
   IF NOT AVAILABLE CLI THEN DO:
      CREATE CLI.
      ASSIGN CLI.CustNum = iiToCust
             CLI.CLI     = ttCLI.CLI
             CLI.Date    = TODAY
             CLI.CrStamp = ldFromStamp
             CLI.ClStamp = ldToStamp.
   END. 

   ELSE ASSIGN CLI.CrStamp = MIN(ldFromStamp,CLI.CrStamp)
               CLI.ClStamp = MAX(ldToStamp,CLI.ClStamp).
   
   /* series */
   FIND FIRST CLISer NO-LOCK WHERE
      CLISer.CustNum  = iiToCust  AND
      CLISer.CLIFrom <= ttCLI.CLI AND
      CLISer.CLITo   >= ttCLI.CLI NO-ERROR.
   IF NOT AVAILABLE CLISer THEN DO:
      CREATE CLISer.
      ASSIGN CLISer.CustNum = iiToCust
             CLISer.CLIFrom = ttCLI.CLI
             CLISer.CLITo   = ttCLI.CLI.
   END.              

   oiCount = oiCount + 1.
END. 

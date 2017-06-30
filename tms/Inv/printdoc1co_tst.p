/* ----------------------------------------------------------------------
  MODULE .......: printdoc1co.p
  TASK .........: Collect invoices to a doc1 file 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.12.06
  CHANGED ......: 22.03.07/aam pick compensation cases
                  12.04.07/aam mms invrows
                  10.05.07/aam vasmtsms
                  15.05.07/aam periodical contract penalty fee
                  03.09.07/aam msisdn change and terminations
                  11.09.07/aam full and reduced amounts separately in
                               penalty fee
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}

DEFINE INPUT  PARAMETER icInvGrp       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum1     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum2     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER icInvID1       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icInvID2       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iiInvDate      AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER ilOnlyNew      AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ilCredit       AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ilOnlyFile     AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iiInvType      AS INT       NO-UNDO.
DEFINE INPUT  PARAMETER icFile         AS CHAR      NO-UNDO. 
DEFINE OUTPUT PARAMETER oiInvCount     AS INT       NO-UNDO.
DEFINE OUTPUT PARAMETER ocError        AS CHAR      NO-UNDO. 

DEF VAR ldtNameDate AS DATE NO-UNDO. 
DEF VAR lcDate      AS CHAR NO-UNDO.
DEF VAR lcCLIFile   AS CHAR NO-UNDO.

{Inv/printdoc1tt.i}


FUNCTION fMakeTemp RETURNS LOGICAL.

    if can-find(first ttinvoice where ttinvoice.invnum = invoice.invnum)
    then return false.
    
    /* printing denied */
    IF Invoice.InvCfg[1] = TRUE THEN DO:
       ocError = "Printing denied".
       RETURN FALSE.
    END.

    ELSE IF ilOnlyNew AND Invoice.PrintState > 0 THEN RETURN FALSE.
    
    ELSE IF NOT ilCredit AND Invoice.InvAmt < 0 THEN DO:
       ocError = "Invoice is a credit invoice".
       RETURN FALSE.
    END.
    
    ELSE IF NOT ilCredit AND Invoice.CrInvNum > 0 AND Invoice.InvAmt >= 0 
    THEN DO:
       ocError = "Invoice has been credited".
       RETURN FALSE.
    END. 
    
    /* invoice type */
    ELSE IF iiInvType > 0 AND Invoice.InvType NE iiInvType 
    THEN RETURN FALSE.
    
    ELSE DO:
    
       CREATE ttInvoice.
       ASSIGN ttInvoice.InvNum = Invoice.InvNum
              oiInvCount       = oiInvCount + 1. 

       IF Customer.IDelName > "" 
       THEN ttInvoice.ZipCode = Customer.IDelZipCode.
       ELSE ttInvoice.ZipCode = Customer.ZipCode.

       IF ldtNameDate = ? THEN ldtNameDate = Invoice.InvDate.

       RETURN TRUE.
    END.
    
END FUNCTION.

def temp-table ttinv no-undo
    field clitype  as char
    field language as int
    field mincons  as int
    field fatime   as char
    field taxzone  as char
    field qty      as int
    field billcode as char
    index language language clitype mincons.
   
def temp-table ttpick no-undo
   field invnum as int
   field cli as char
   index cli cli
   index invnum invnum.

def temp-table ttcli no-undo
   field cli as char.
   
def var i       as int  no-undo.
def var j       as int  no-undo.
def var limin   as int  no-undo.
def var lczone  as char no-undo.
def var licli   as int  no-undo.
def var litake  as int  no-undo init 50.
def var lcfat   as char no-undo.
def var lcitems as char no-undo.
def var lcbcode as char no-undo.
def var ldfrom  as dec  no-undo.
def var ldto    as dec  no-undo.
def var lctype  as char no-undo.
def var liseq   as int  no-undo.
def var lcread  as char no-undo.
def var lcfatrow as char no-undo.

lcCLIFile = "/scratch/print/testinv/test_msisdn.txt".

def stream scli.
if search(lcclifile) ne ? then do:
   input stream scli from value(lcclifile).
   
   repeat:
      import stream scli unformatted lcread.
      
      create ttcli.
      ttcli.cli = lcread.
   end.
   
   input stream scli close.

end.


def buffer binv for invoice.
def buffer bsubinv for subinvoice.

IF NOT ilOnlyFile THEN DO:

for each billitem no-lock where
         billitem.brand = "1" and
         billitem.biname begins "mms":
   lcitems = lcitems + (if lcitems > "" then "," else "") + 
             billitem.billcode.
end.

lcitems = lcitems + ",VASMTSMS,TERMPERIOD".

/* gather only test invoices */
for each invoice no-lock where
         invoice.brand   = "1"       and
         invoice.invdate = iiInvDate and
         invoice.invtype = 99,
   first customer of invoice no-lock where
         customer.language > 0:
   

   IF ilOnlyNew AND Invoice.PrintState > 0 THEN NEXT.

   /* limin; in all cases greater nbr means 'with minimum consumption' 
      0-1 plain subscr.
      2-3 terminated 
      4-5 msisdn change
      6-7 owner change
      8-9 more than 1 invoice on period 
   */
   
   limin = integer(can-find(first invrow of invoice where
                                  invrow.billcode = "MCCONTR1")).

   ldfrom = year(invoice.fromdate) * 10000 + 
            month(invoice.fromdate) * 100 + 
            day(invoice.fromdate).
   ldto   = year(invoice.todate) * 10000 + 
            month(invoice.todate) * 100 + 
            day(invoice.todate) + 0.86399.

   lcbcode = "".
   for each invrow of invoice no-lock:
      if lookup(invrow.billcode,lcitems) > 0 then do:
         lcbcode = invrow.billcode.
         
         /* full and reduced amounts separately */   
         IF invrow.billcode = "TERMPERIOD" and invrow.amt = 100
         THEN lcbcode = lcbcode + "100". 
         
         leave.
      end.
   end.
   
   find region where region.region = customer.region no-lock no-error.
   if available region then lczone = region.taxzone.
   else lczone = "".
                    
   if lczone = "" then next. 
                         
   assign licli = 0
          lcfat = "".
          
   for each invrow of invoice no-lock where
            invrow.rowtype = 7:
            
         for first fatime no-lock where
                   fatime.invnum   = invoice.invnum,
             first fatgroup of fatime no-lock where
                   fatgroup.billcode = invrow.billcode:
               
            lcfatrow = fatgroup.ftgrp +         
                       /* used for the first time on this invoice */
                       if fatime.origfat = 0 then "x" else "".
            if lookup(lcfatrow,lcfat) > 0 then next.
            lcfat = lcfat + (if lcfat > "" then "," else "") + lcfatrow.
         end.
   end.
   
   for each SubInvoice of Invoice no-lock:
             
      find mobsub where mobsub.cli = subinvoice.cli no-lock no-error.
      if available mobsub then lctype = mobsub.clitype. 

      else do:
   
         for first msowner no-lock where
                   msowner.msseq = subinvoice.msseq and
                   msowner.tsbeg <= ldto and
                   msowner.tsend >= ldfrom:
            lctype = msowner.clitype.
         end.

         /* terminated */
         IF CAN-FIND(FIRST MsRequest WHERE
                           MsRequest.MsSeq     = subInvoice.MsSeq   AND
                           MsRequest.ReqType   = 18              AND
                           MsRequest.ReqStat   = 2               AND 
                           MsRequest.ActStamp >= ldFrom       AND
                           MsRequest.ActStamp <= ldTo) 
         THEN limin = limin + 2.
      end.
   
      if limin <= 1 then do:
      
         /* owner change */           
         liseq = 0.
         for each MsOwner no-lock WHERE
                  Msowner.Brand  = gcBrand     AND
                  MsOwner.CLI    = subinvoice.cli AND
                  MsOwner.TsEnd >= ldFrom      AND
                  MsOwner.TsBeg <  ldTo
         by msowner.tsend:
                
            if liseq = 0 then do:
               liseq = msowner.msseq.
               next.
            end.
            
            if liseq ne msowner.msseq then do:
               limin = limin + 6.
               leave.
            end.
         end.

         /* msisdn change */           
         if limin <= 1 then 
         for each MsOwner no-lock WHERE
                  MsOwner.msseq  = subinvoice.msseq AND
                  MsOwner.TsEnd >= ldFrom        AND
                  MsOwner.TsBeg <  ldTo
         by msowner.tsend:
                
            if msowner.cli ne subinvoice.cli then do:
               limin = limin + 4.
               leave.
            end.
         end.

      END.
 
      /* more than 1 invoice on period */
      IF limin <= 1 THEN DO:
   
         for each bsubinv no-lock where
                  bsubinv.msseq = subinvoice.msseq,
            first binv of bsubinv no-lock where 
                  binv.invdate = iiInvDate   and
                  binv.invtype = 99          and
                  binv.invnum  ne invoice.invnum:
            limin = limin + 8.
            leave.
         end.
      END.
   
      find first ttinv where
                 ttinv.language = customer.language and
                 ttinv.clitype  = lctype            and
                 ttinv.mincons  = limin             and
                 ttinv.fatime   = lcfat             and
                 ttinv.taxzone  = lczone            and
                 ttinv.billcode = lcbcode no-error.
      if not available ttinv then do:
         create ttinv.
         assign ttinv.language = customer.language
                ttinv.clitype  = lctype
                ttinv.mincons  = limin
                ttinv.fatime   = lcfat
                ttinv.taxzone  = lczone
                ttinv.billcode = lcbcode.
      end.
             
      if ttinv.qty < litake then do:
         create ttpick.
         assign ttpick.invnum = invoice.invnum
                ttpick.cli    = subinvoice.cli
                ttinv.qty     = ttinv.qty + 1
                j             = j + 1.
      end.

      i = i + 1.
      pause 0.
      disp i j with 1 down.
   end.
end.

END.  /* ilOnlyFile */

/* make sure that all in cli list were picked */
for each ttcli:
   if can-find(first ttpick where ttpick.cli = ttcli.cli) then next.
   
   for each invoice no-lock where
            invoice.brand = gcbrand and
            invoice.invdate = iiInvDate and
            invoice.invtype = 99,
      first subinvoice of invoice no-lock where
            subinvoice.cli = ttcli.cli:

      create ttpick.
      assign ttpick.invnum = invoice.invnum
             ttpick.cli    = subinvoice.cli.
   end.

end.

for each ttpick,
   first invoice no-lock where
         invoice.invnum = ttpick.invnum,
   first customer of invoice no-lock:
         
   fMakeTemp().

end.

/* invgroup to file name */
IF icInvGrp > "" 
THEN icFile = REPLACE(icFile,"#IGRP",icInvGrp).
ELSE icFile = REPLACE(icFile,"#IGRP","ALL").
   
/* invoice date to file name */   
IF ldtNameDate NE ? THEN DO:
   
   lcDate = DYNAMIC-FUNCTION("fDateFmt" IN ghFunc1,
                             ldtNameDate,
                             "yyyymmdd").
   icFile = REPLACE(icFile,"#IDATE",lcDate).
END.
ELSE icFile = REPLACE(icFile,"#IDATE","").

/* print */
RUN printdoc1 (INPUT-OUTPUT TABLE ttInvoice,  
               oiInvCount,
               icFile,
               "",
               0,
               0,
               0,
               OUTPUT oiInvCount). 

ocError = RETURN-VALUE.
 

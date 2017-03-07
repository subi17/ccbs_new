/* ------------------------------------------------------
  MODULE .......: ACCDATLI.P
  FUNCTION .....: List accounting data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.03.01
  MODIFIED .....: 02.04.01/aam optionally TO excel-file
                  20.02.02/aam NEW logic
                  01.08.02/aam customer nbrs, 
                               day for unbilled separately from situation date
                  12.09.02/aam optionally divide unbilled into months
                  18.12.02/aam optionally to excel file 
                  09.04.03/aam leave out groups that are not invoiced
                  15.09.03/aam brand
                  27.05.04/aam vat percent
                  31.05.04/aam AccRid
                  10.06.04/aam new file format for Sap
                  19.05.04/aam several printing formats with one run
                  20.12.05/aam unbilled cdrs with pCurrentMonthCDRs
                  30.03.06/aam sort according to VatPerc, not VatCode
                  28.08.06/aam ilUBCalls, ilUBOldCalls, ilUBFees
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

/* print-linemuuttujat */
{Syst/utumaa.i}

&GLOBAL-DEFINE UseEventDate NO
&GLOBAL-DEFINE GetOnlySums NO
{Ar/accdatfi.i}

DEF INPUT PARAMETER iDate1       AS Date  NO-UNDO.
DEF INPUT PARAMETER iDate2       AS Date  NO-UNDO.
DEF INPUT PARAMETER iCustNum1    AS INT   NO-UNDO.
DEF INPUT PARAMETER iCustNum2    AS INT   NO-UNDO.
DEF INPUT PARAMETER iIgcode1     AS CHAR  NO-UNDO.
DEF INPUT PARAMETER iIgcode2     AS CHAR  NO-UNDO.
DEF INPUT PARAMETER iIgSplit     AS LOGIC NO-UNDO. 
DEF INPUT PARAMETER iBilled      AS LOGIC NO-UNDO.
DEF INPUT PARAMETER iUnbilled    AS LOGIC NO-UNDO.
DEF INPUT PARAMETER idtUnbilled  AS DATE  NO-UNDO. 
DEF INPUT PARAMETER ilMonthly    AS LOGIC NO-UNDO. 
DEF INPUT PARAMETER ilUBCalls    AS LOGIC NO-UNDO. 
DEF INPUT PARAMETER ilUBOldCalls AS LOGIC NO-UNDO.
DEF INPUT PARAMETER ilUBFees     AS LOGIC NO-UNDO. 
DEF INPUT PARAMETER iPeriodized  AS LOGIC NO-UNDO.
DEF INPUT PARAMETER ilPaper      AS LOG   NO-UNDO. 
DEF INPUT PARAMETER ilExcel      AS LOG   NO-UNDO. 
DEF INPUT PARAMETER icExFile     AS CHAR  NO-UNDO.
DEF INPUT PARAMETER ilSap        AS LOG   NO-UNDO. 
DEF INPUT PARAMETER icSapFile    AS CHAR  NO-UNDO.

DEF TEMP-TABLE wPrint NO-UNDO
    FIELD InvGroup1 AS CHAR
    FIELD InvGroup2 AS CHAR
    FIELD IGName  AS CHAR. 

DEF VAR viiva1 as char format "x(91)" NO-UNDO.
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR sl     AS INT  NO-UNDO.
DEF VAR rl     AS INT  NO-UNDO.
DEF VAR rlx    AS INT  NO-UNDO.
DEF VAR lev    AS INT  NO-UNDO INIT 90.
DEF VAR yxnimi AS CHAR NO-UNDO. 
DEF VAR otsi   AS CHAR NO-UNDO EXTENT 39.

DEF VAR xOk         AS LOG  NO-UNDO.
DEF VAR xEmpty      AS CHAR NO-UNDO INIT "<EMPTY>".
DEF VAR xSessionNum AS CHAR NO-UNDO.
DEF VAR xHeader     AS CHAR NO-UNDO.
DEF VAR xDateHeader AS CHAR NO-UNDO.
DEF VAR xProdLines  AS LOG  NO-UNDO.
DEF VAR lcAddtHead  AS CHAR NO-UNDO. 
DEF VAR lcExFile    AS CHAR NO-UNDO.    
DEF VAR lcVatHeader AS CHAR NO-UNDO. 
DEF VAR ldPrintAmt  AS DEC  NO-UNDO. 
DEF VAR lcSapVat    AS CHAR NO-UNDO. 
DEF VAR TAB         AS CHAR NO-UNDO.
DEF VAR MY-NL       AS CHAR NO-UNDO. 
DEF VAR ldtCdrDate  AS DATE NO-UNDO.

DEF STREAM sExcel.
DEF STREAM sSap.


form header
   viiva1 AT 1 SKIP
   yxnimi at 1 format "x(35)" 
      xHeader + " REVENUE" at 40 FORMAT "X(40)" 
      "Page" AT 81  
      sl format "ZZZZ9" SKIP
   lcAddtHead AT 1 FORMAT "X(10)"
      lcVatHeader AT 15 FORMAT "X(10)"
      xDateHeader AT 40 FORMAT "X(35)"
      pvm format "99.99.9999" AT 81 SKIP
   viiva2 AT 1 skip(1)
   WITH width 95 NO-LABEL no-box FRAME sivuotsi.

form header
   "Account"  TO 8
   "Name"     AT 10
   "VAT"      TO 36 
   "CCentre"  AT 38
   "CCName"   AT 47
   "Category" AT 66
   "Amount"   TO 90
   SKIP
   viiva3 AT 1 
   SKIP
WITH width 95 NO-LABEL no-box FRAME sarotsi.

/* change the page */
FUNCTION CheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF NOT ilPaper THEN RETURN FALSE. 

    IF rl >= skayt1 - iAddLine THEN DO:
        {Syst/uprfeed.i rl}
        ASSIGN rlx = 0
               sl = sl + 1.
        view STREAM tul FRAME sivuotsi.  
        ASSIGN rl = 12.
        view STREAM tul FRAME sarotsi.   
        ASSIGN rl = rl + 2.
    END.

    RETURN TRUE.
END.

FUNCTION fDateFormat RETURNS CHARACTER
   (idtDate AS DATE).
   
   IF idtDate = ? THEN RETURN "".
   
   RETURN STRING(YEAR(idtDate) MOD 100,"99") +
          STRING(MONTH(idtDate),"99") +
          STRING(DAY(idtDate),"99").
   
END.

/* print the beginning of the line (ALL but the amount) */
FUNCTION fPrintAccount RETURNS LOGICAL.

        FIND Account WHERE 
             Account.Brand  = gcBrand AND
             Account.AccNum = wAccData.AccNum NO-LOCK NO-ERROR.
        FIND CostCentre WHERE
             CostCentre.Brand      = gcBrand AND
             CostCentre.CostCentre = wAccData.CostCentre NO-LOCK NO-ERROR.
        FIND CustCat WHERE
             CustCat.Brand   = gcBrand AND
             CustCat.Category = wAccData.Category NO-LOCK NO-ERROR.
        
        IF ilPaper THEN DO:
           CheckPage(0). 

           PUT STREAM tul 
            wAccData.AccNum     TO 8 FORMAT ">>>>>9"
            (IF AVAILABLE Account
             THEN Account.AccName
             ELSE "")           AT 10 FORMAT "X(22)"
            (IF wAccData.VatPerc > 0
             THEN STRING(wAccData.VatPerc,">9.9")
             ELSE "")           TO 36 FORMAT "X(4)"
            wAccData.CostCentre AT 38 FORMAT "X(8)"
            (IF AVAILABLE CostCentre
             THEN CostCentre.CCName 
             ELSE "")           AT 47 FORMAT "X(18)"
            (IF AVAILABLE CustCat
             THEN CustCat.CatName
             ELSE wAccData.Category)
                                AT 66 FORMAT "X(8)".
        END.
        
        IF ilExcel THEN 
        PUT STREAM sExcel UNFORMATTED
            (IF ilMonthly AND wAccData.EType = "U" AND wAccData.Month > 0
             THEN STRING(wAccData.Month)
             ELSE "")           TAB
            wAccData.AccNum     TAB
            (IF AVAILABLE Account
             THEN Account.AccName
             ELSE "")           TAB
            (IF wAccData.VatPerc > 0
             THEN STRING(wAccData.VatPerc,">9.9")
             ELSE "")           TAB
            wAccData.CostCentre TAB
            (IF AVAILABLE CostCentre
             THEN CostCentre.CCName 
             ELSE "")           TAB
            (IF AVAILABLE CustCat
             THEN CustCat.CatName
             ELSE wAccData.Category) TAB
            wAccData.AccRid          TAB .
        
       
END FUNCTION.

ASSIGN 
   xSessionNum = SESSION:NUMERIC-FORMAT
   SESSION:NUMERIC-FORMAT = "European"
   viiva1      = fill("=",lev)
   viiva2      = fill("=",lev)
   viiva3      = fill("-",lev)
   viiva4      = fill("-",lev)
   lcVatHeader = "VAT " + (IF llSepVat THEN "Excl." ELSE "Incl.")
   TAB         = CHR(9)
   MY-NL       = CHR(10)
   sl          = 1
   rl          = 0.

/* make a TEMP-TABLE FIRST 
   handle one InvGroup AT a time -> faster retrieve when
   only one wanted 
*/

ldtCDRDate = iDate2.

EMPTY TEMP-TABLE ttMCDR.

/* if unbilled section wanted for one month then get all cdrs from that month,
   because assumption is that cdrs will be billed during next month
   (cdrs are mainly in date/time order in db -> quicker to read them this way)
*/   
IF iUnbilled AND ilUBCalls       AND 
   MONTH(iDate1) = MONTH(iDate2) AND
   YEAR(iDate1)  = YEAR(iDate2) THEN DO:
 
   ldtCDRDate = iDate1 - 1.
   
   RUN pCurrentMonthCDRs (iDate1,
                          iDate2,
                          iIgCode1,
                          iIgCode2).
END.

EMPTY TEMP-TABLE ttInvoice.

/* get new invoices to temp-table, so that unbilled-section doesn't have
   to check them for each event */
IF iUnbilled THEN 
FOR EACH Invoice NO-LOCK WHERE
         Invoice.Brand   = gcBrand AND
         Invoice.InvDate > idtUnbilled:
         
   IF LOOKUP(STRING(Invoice.InvType),lcTypeDen) = 0 THEN DO:
      CREATE ttInvoice.
      ttInvoice.InvNum = Invoice.InvNum.
   END. 
END.


FOR EACH InvGroup NO-LOCK WHERE
         InvGroup.Brand     = gcBrand  AND
         InvGroup.InvGroup GE iIgcode1 AND
         InvGroup.InvGroup LE iIgcode2:

    /* if not specifically defined (one group limited) then leave out
       groups that are not invoiced */
    IF iIgCode1 NE iIgCode2 AND
       InvGroup.BillPerm = FALSE
    THEN NEXT. 

     /* don't combine inv.groups */
    IF iIgSplit THEN DO:
       CREATE wPrint.
       ASSIGN wPrint.InvGroup1 = InvGroup.InvGroup
              wPrint.InvGroup2 = InvGroup.InvGroup
              wPrint.IGName  = InvGroup.IGName.
    END.

    /* Unbilled data */
    IF iUnbilled THEN 
    RUN FindUnBilled(iDate2,
                     ldtCdrDate,
                     idtUnbilled,
                     ilUBCalls,
                     ilUBOldCalls,
                     ilUBFees,
                     InvGroup.InvGroup,
                     iCustNum1,
                     iCustNum2).

    /* Billed data and/or periodized data */
    IF iBilled OR iPeriodized THEN 
    RUN FindInvoices(iBilled,
                     iPeriodized,
                     iDate1, 
                     iDate2,
                     InvGroup.InvGroup,
                     iCustNum1, 
                     iCustNum2).
END.


/* combine invoicing groups */
IF NOT iIgSplit THEN DO:
    CREATE wPrint.
    ASSIGN wPrint.InvGroup1 = iIgcode1
           wPrint.InvGroup2 = iIgcode2
           wPrint.IGName    = ynimi.
END.

/* 
print the list only after ALL events have been gathered  
-> IF Printed TO paper THEN the list comes without brakes 
*/
FOR EACH wPrint
BY wPrint.InvGroup1:

    IF ilExcel THEN DO:

       lcExfile = icExfile +
                  /* separate files */
                  IF iIGSplit
                  THEN "_" + wPrint.InvGroup1 
                  ELSE "".

       /* if not iIGSplit then there is only one wPrint -> no need to use
          "append" */
       OUTPUT STREAM sExcel TO VALUE(lcExfile). 
    END.

    IF ilSap THEN DO:

       lcExfile = icSapFile +
                  /* separate files */
                  IF iIGSplit
                  THEN "_" + wPrint.InvGroup1 
                  ELSE "".

       /* if not iIGSplit then there is only one wPrint -> no need to use
          "append" */
       OUTPUT STREAM sSap TO VALUE(lcExfile). 
    END.


    /* contains group Name when groups are split */
    ASSIGN yxnimi = wPrint.IGName.

    /* Billed events */
    IF iBilled THEN DO:
        ASSIGN xHeader     = "BILLED"
               xDateHeader = STRING(iDate1,"99.99.9999") + " - " +
                             STRING(iDate2,"99.99.9999").

        IF NOT iIgSplit OR
           /* IF groups are splitted THEN InvGroup1 = InvGroup2 */
           CAN-FIND(FIRST wAccData WHERE
                          wAccData.Etype  = "B" AND
                          wAccData.Igcode = wPrint.InvGroup1)
        THEN RUN PrintAccLines("B",
                               wPrint.InvGroup1,
                               wPrint.InvGroup2,
                               FALSE).
    END.

    /* Unbilled events */
    IF iUnbilled THEN DO:
        ASSIGN xHeader     = "UNBILLED"
               xDateHeader = "Until " + STRING(iDate2,"99.99.9999") +
                             " (" + STRING(idtUnbilled,"99.99.9999") + ")".


        IF NOT iIgSplit OR
           /* IF groups are NOT splitted THEN InvGroup1 = InvGroup2 */
           CAN-FIND(FIRST wAccData WHERE
                          wAccData.Etype  = "U" AND
                          wAccData.Igcode = wPrint.InvGroup1)
        THEN DO:

           lcAddtHead = "".

           /* first divided into months */
           IF ilMonthly THEN DO:
              lcAddtHead = "Monthly".
              RUN PrintAccLines("U",
                                wPrint.InvGroup1,
                                wPrint.InvGroup2,
                                TRUE).
              lcAddtHead = "Totals".
           END.

           /* then totals */
           RUN PrintAccLines("U",
                             wPrint.InvGroup1,
                             wPrint.InvGroup2,
                             FALSE).
           lcAddtHead = "". 
       END. 

    END.

    IF iPeriodized THEN DO:
        /* periodized events */
        ASSIGN xHeader     = "PERIODIZED"
               xDateHeader = "From " + STRING(iDate2 + 1,"99.99.9999") + 
                             " onwards".

        IF NOT iIgSplit OR
           /* IF groups are NOT splitted THEN InvGroup1 = InvGroup2 */
           CAN-FIND(FIRST wAccData WHERE
                          wAccData.Etype  = "PD" AND
                          wAccData.Igcode = wPrint.InvGroup1)
        THEN RUN PrintAccLines("PD",
                               wPrint.InvGroup1,
                               wPrint.InvGroup2,
                               FALSE).
    END.

    IF ilExcel THEN OUTPUT STREAM sExcel CLOSE.
    IF ilSap   THEN OUTPUT STREAM sSap   CLOSE.

END.

ASSIGN SESSION:NUMERIC-FORMAT = xSessionNum.


PROCEDURE PrintAccLines:

  DEF INPUT PARAMETER iType      AS CHAR  NO-UNDO. 
  DEF INPUT PARAMETER iIgcode1   AS CHAR  NO-UNDO.
  DEF INPUT PARAMETER iIgcode2   AS CHAR  NO-UNDO. 
  DEF INPUT PARAMETER ilMthSplit AS LOGIC NO-UNDO. 

  IF ilPaper THEN DO:
      assign sl = 1. 
      VIEW STREAM tul FRAME sivuotsi.
      VIEW STREAM tul FRAME sarotsi.
      ASSIGN rl = 14.
  END.

  IF ilExcel THEN DO:
      
      PUT STREAM sExcel UNFORMATTED
         yxnimi                 MY-NL
         xHeader + " REVENUE "  
         lcAddtHead             MY-NL
         xDateHeader            MY-NL
         lcVatHeader            MY-NL
         "Printed on " + STRING(pvm,"99.99.9999") 
         MY-NL
         MY-NL.

      PUT STREAM sExcel UNFORMATTED
         (IF ilMthSplit AND iType = "U" 
          THEN "Month" 
          ELSE "")              TAB
         "Account"              TAB
         "Name"                 TAB
         "VAT"                  TAB
         "CCentre"              TAB
         "CC Name"              TAB
         "Category"             TAB
         "Reporting ID"         TAB 
         "Amount"   
         MY-NL.
  END.

  /* remove month division ("by" phrase could of course use 
     "if ilMthSplit then .." but this makes the printing part simpler)
  */
  IF NOT ilMthSplit THEN 
  FOR EACH wAccData WHERE
           wAccData.Etype  =  iType    AND
           wAccData.Igcode GE iIgcode1 AND
           wAccData.Igcode LE iIgcode2:
     wAccData.Month = 0. 
  END.

  IF NOT ilExcel AND NOT ilSap THEN 
  FOR EACH wAccData WHERE
           wAccData.Etype  =  iType    AND
           wAccData.Igcode GE iIgcode1 AND
           wAccData.Igcode LE iIgcode2:
     wAccData.AccRid = "". 
  END.

  /* collected data */
  FOR EACH wAccData WHERE
           wAccData.Etype  =  iType    AND
           wAccData.Igcode GE iIgcode1 AND
           wAccData.Igcode LE iIgcode2 AND
           wAccData.Amount NE 0
  BREAK 
  BY wAccData.Month
  BY wAccData.AccNum 
  BY wAccData.CostCentre
  BY wAccData.AccRid
  BY wAccData.VatPerc
  BY wAccData.Category 
  BY wAccData.BillCode:

    /* divide by month */
    IF ilMthSplit               AND 
       FIRST-OF(wAccData.Month) AND
       ilPaper
    THEN DO:

       CheckPage(3).

       PUT STREAM tul UNFORMATTED
          "Month: " AT 1
          wAccData.Month
          SKIP.
       ASSIGN rl = rl + 1. 

    END. 

    ACCUMULATE wAccData.Amount (TOTAL BY wAccData.Month
                                      BY wAccData.AccNum
                                      BY wAccData.Category
                                      BY wAccData.BillCode).

    /* sums AT customer category Level */        
    IF LAST-OF(wAccData.Category) THEN DO:

        fPrintAccount().

        IF ilPaper THEN DO:
           PUT STREAM tul 
              {Ar/accdatli.i "BY wAccData.Category"}
           SKIP.
           ASSIGN rl = rl + 1.
        END. 

        IF ilExcel THEN PUT STREAM sExcel UNFORMATTED 
           ROUND((ACCUM TOTAL BY wAccData.Category wAccData.Amount),2)
           MY-NL.

        /* sap-file */
        IF ilSap THEN DO:

           ldPrintAmt = 100 * ROUND((ACCUM TOTAL BY wAccData.Category
                                                    wAccData.Amount),2).
                            
           /* sap vatcode */ 
           CASE wAccData.VatPerc:
           WHEN 0.00  THEN lcSapVat = "Z1".
           WHEN 8.00  THEN lcSapVat = "A8".
           WHEN 22.00 THEN lcSapVat = "A1".
           OTHERWISE lcSapVat = "".
           END CASE.
           
           /* cost centre for revenue accounts */
           IF AVAILABLE Account AND Account.AccType = 12 
           THEN wAccData.CostCentre = "20260".
                                             
           PUT STREAM sSap UNFORMATTED                 
              "00"                                     /* empty */
              "00"                                     /* document type */
              "00000001"                               /* document nbr */
              STRING(YEAR(iDate2) MOD 100,"99")        /* period year*/
              STRING(MONTH(iDate2),"99")               /* period month */
              "0427"                                   /* company code */
              fDateFormat(TODAY)                       /* document date */
              fDateFormat(iDate2)                      /* posting date */
              SPACE(30)                                /* document txt */
              STRING(wAccData.AccNum,"99999999")       /* account */
              FILL("0",6)                              /* subaccount */
              STRING(lcSapVat,"x(3)")                  /* vat */
              STRING(ABS(ldPrintAmt),"99999999999")    /* posting */
              (IF ldPrintAmt < 0 THEN "-" ELSE " ")    /* sign */
              STRING(ldPrintAmt,"99999999999-")        /* posting */
              "EUR"                                    /* currency */
              SPACE(30)                                /* text */
              SPACE(2)                                 /* document type */
              FILL("0",8)                              /* co account */
              "2026"                                   /* profit center */
              SPACE(13)                                /* order nbr */
              STRING(wAccData.AccRid,"X(7)")           /* sap product */
              STRING(wAccData.CostCentre,"X(6)")       /* cost centre */
              SPACE(7)                                 /* country etc. */
              FILL("0",11)                             /* quantity */
              SPACE(1)                                 /* sign */
              FILL("0",12)                             /* quantity 2 */
              SPACE(4)                                 /* location */
              STRING(ABS(ldPrintAmt),"99999999999999") /* currency posting */
              (IF ldPrintAmt < 0 THEN "-" ELSE " ")    /* sign */
              SKIP.
                                                      
        END.
 
        ACCUMULATE wAccData.Amount (COUNT BY wAccData.AccNum). 
    END.

    /* sub-total BY AccNum (IF more than 1 line per AccNum) */
    IF LAST-OF(wAccData.AccNum) THEN DO:

        IF (ACCUM COUNT BY wAccData.AccNum wAccData.Amount) GT 1 
        THEN DO:

            IF ilPaper THEN DO:
               CheckPage(3).

               PUT STREAM tul UNFORMATTED
                  FILL("-",90) AT 1 SKIP
                  wAccData.AccNum TO 8
                  " total"  
                  {Ar/accdatli.i "BY wAccData.AccNum"}
                  SKIP(1).

               ASSIGN rl = rl + 3.
            END.

            IF ilExcel THEN DO:

               PUT STREAM sExcel UNFORMATTED 
                                       TAB
                  wAccData.AccNum      TAB
                  "Total"              TAB
                                       TAB
                                       TAB
                                       TAB
                                       TAB
                                       TAB
                  ROUND((ACCUM TOTAL BY wAccData.AccNum wAccData.Amount),2)
                  MY-NL
                  MY-NL.
            END.
        END.

        ELSE DO:

           IF ilPaper THEN DO:
              CheckPage(0).

              PUT STREAM tul SKIP(1).
              ASSIGN rl = rl + 1.
           END. 

           IF ilExcel THEN PUT STREAM sExcel MY-NL.

        END.       
    END.

    /* sub-total by month */
    IF ilMthSplit              AND
       LAST-OF(wAccData.Month) AND
       ilPaper
    THEN DO:

       CheckPage(3).

       PUT STREAM tul UNFORMATTED
          FILL("-",90) AT 1 SKIP
          wAccData.Month AT 1
          " total"  
          {Ar/accdatli.i "BY wAccData.Month"}
          SKIP(1).

        ASSIGN rl = rl + 3.
    END.

    /* grand total */
    IF LAST(wAccData.AccNum) THEN DO:

        IF ilPaper THEN DO:
           CheckPage(2).

           PUT STREAM tul UNFORMATTED
              FILL("=",90) AT 1 SKIP
              "Total"  
              {Ar/accdatli.i}
              SKIP.

           ASSIGN rl = rl + 2.
        END.

        IF ilExcel THEN  DO:
           PUT STREAM sExcel UNFORMATTED MY-NL.

           PUT STREAM sExcel UNFORMATTED
                      TAB
              "TOTAL" TAB
                      TAB
                      TAB
                      TAB
                      TAB
                      TAB
                      TAB
              ROUND((ACCUM TOTAL wAccData.Amount),2)
              MY-NL
              MY-NL.
        END.

     END.

  END. /* foreach */

  IF ilPaper THEN DO:
     {Syst/uprfeed.i rl}
  END.

END PROCEDURE. 



 /* ------------------------------------------------------
  MODULE .......: accdatll.p
  FUNCTION .....: ui FOR revenue report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.03.01
  MODIFIED .....: 02.04.01/aam optionally TO excel-file
                  20.02.02/aam NEW Version FOR JG 
                  01.08.02/aam customer nbrs, 
                               day for unbilled separately from situation date
                  02.09.02/aam better help-text for ldtUnbilled
                  12.09.02/aam optionally divide unbilled into months
                  18.12.02/aam optionally to excel file 
                  15.09.03/aam brand
                  10.06.04/aam optionally to Sap file
                  19.05.04/aam several printing formats with one run
                  30.01.06/aam write to eventlog (external)               
                  17.05.06/aam separate field for directory of files,
                               duration of run
                  28.08.06/aam choice for taking calls and fees              
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/utumaa.i "new"}
{Syst/eventlog.i}
{Func/cparam2.i}
{Func/timestamp.i}

assign tuni1 = "accdatll"
       tuni2 = "".

DEF VAR ufkey      AS LOG                     NO-UNDO.
def var ok         as log   format "Yes/No"   NO-UNDO.
def var date1       as Date  format "99-99-99" NO-UNDO.
def var date2       as Date  format "99-99-99" NO-UNDO.
DEF VAR liCust-nr1  AS INT   FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR liCust-nr2  AS INT   FORMAT ">>>>>>>9" NO-UNDO. 

DEF VAR xProdLines  AS LOGIC FORMAT "Yes/No"          NO-UNDO.
DEF VAR xBilled     AS LOGIC FORMAT "Yes/No"          NO-UNDO INIT TRUE.
DEF VAR xUnbilled   AS LOGIC FORMAT "Yes/No"          NO-UNDO INIT TRUE.
DEF VAR xPeriodized AS LOGIC FORMAT "Yes/No"          NO-UNDO INIT TRUE.
DEF VAR lcExfile    AS CHAR  FORMAT "X(40)"           NO-UNDO.
DEF VAR lcSapFile   LIKE lcExFile.
DEF VAR InvGroup    AS CHAR  FORMAT "X(8)"            NO-UNDO EXTENT 2.
DEF VAR xIgSplit    AS LOGIC FORMAT "Yes/No"          NO-UNDO INIT TRUE. 
DEF VAR ldtUnbilled AS DATE  FORMAT "99-99-99"        NO-UNDO. 
DEF VAR llMthSplit  AS LOGIC FORMAT "Monthly/Totals"  NO-UNDO INIT TRUE. 
DEF VAR llSap       AS LOGIC FORMAT "Yes/No"          NO-UNDO INIT FALSE.
DEF VAR llExcel     LIKE llSap.
DEF VAR llPaper     LIKE llSap.

DEF VAR lcLogLine    AS CHAR NO-UNDO. 
DEF VAR lcFileDir    AS CHAR NO-UNDO.
DEF VAR ldBegTime    AS DEC  NO-UNDO.
DEF VAR ldEndTime    AS DEC  NO-UNDO. 
DEF VAR liDurDays    AS INT  NO-UNDO.
DEF VAR liDurTime    AS INT  NO-UNDO.
DEF VAR ldtStartDate AS DATE NO-UNDO.
DEF VAR lcStartTime  AS CHAR NO-UNDO.
DEF VAR llCalls      AS LOG  NO-UNDO.
DEF VAR llFees       AS LOG  NO-UNDO.
DEF VAR llOldCalls   AS LOG  NO-UNDO.


form
   skip(1)
   date1 COLON 20  
      label "Period" 
      help "Time Period for the report"
   "-" 
   date2 
      NO-LABEL 
      help "Time Period for the report" 
   SKIP
   InvGroup[1] COLON 20
        LABEL "Inv. group"
        HELP "Invoicing group"
   "-" 
   InvGroup[2] 
        NO-LABEL
        HELP "Invoicing group"
        SPACE(2)
   SKIP
   xIgSplit COLON 20 
        LABEL "Separate lists"
        HELP "Separate list for each invoicing group"

   liCust-nr1 COLON 20
        LABEL "Customers"
        HELP "Customer numbers"
   "-"
   liCust-nr2 
        NO-LABEL
        HELP "Customer numbers"
        VALIDATE(input liCust-nr2 >= input liCust-nr1,
                 "Lower limit cannot be bigger than the upper limit")
   SKIP(1)

   xBilled COLON 20 
        LABEL "Billed" 
        HELP "List Billed revenue" 
   llCalls COLON 65
        LABEL "Calls To UB"
        HELP "Include calls (cdrs) to unbilled section"
        FORMAT "Yes/No"
   SKIP

   xUnBilled COLON 20 LABEL "Unbilled" 
        HELP "List unbilled revenue" 
      ldtUnbilled 
        NO-LABEL 
        HELP "Invoices are noted till this date"
      llMthSplit
        NO-LABEL
        HELP 
        "Divide unbilled into (M)onthly sums or display only (T)otals"
   llOldCalls COLON 65
        LABEL "Old Calls To UB"
        HELP "Take also older calls, dated before given period's beginning"
        FORMAT "Yes/No"
   SKIP
   
   xPeriodized COLON 20 LABEL "Periodized" 
        HELP "List Periodized revenue" 
   llFees COLON 65
        LABEL "Fees To UB"
        HELP "Include monthly and single fees to unbilled section"
        FORMAT "Yes/No"
   SKIP(1)

   llPaper  COLON 20 LABEL "Paper Print" 
        HELP "Print output to paper" SKIP
   llExcel  COLON 20 LABEL "Excel File" 
        HELP "Make a tab separated file" 
      lcExfile NO-LABEL 
        HELP "File name (without path)"
   llSap  COLON 20 LABEL "SAP File"
        HELP "Make a file in SAP format" 
      lcSapFile NO-LABEL
        HELP "File name (without path)"
   lcFileDir COLON 20
        LABEL "Directory"
        HELP "Directory for files"
        FORMAT "X(50)"
   SKIP(1)     
   
   ldtStartDate COLON 20
        LABEL "Printing Started"
        FORMAT "99-99-99"
   lcStartTime 
        NO-LABEL
   SKIP(1)
   WITH ROW 1 side-labels width 80
        title " " + ynimi + " REVENUE REPORT " +
        string(pvm,"99-99-99") + " "
        FRAME rajat.

view FRAME rajat.
PAUSE 0 no-message.

/* previous MONTH AS default */
ASSIGN date2       = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       date1       = DATE(MONTH(date2),1,YEAR(date2))
       llCalls     = TRUE
       llOldCalls  = TRUE
       llFees      = TRUE
       llExcel     = TRUE
       llSap       = TRUE
       liCust-nr2  = 99999999
       ldtUnbilled = date2
       lcFileDir   = fCParamC("RevenueFileDir").

IF lcFileDir = ? OR lcFileDir = "" THEN lcFileDir = "/tmp".       

FIND LAST InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE InvGroup THEN ASSIGN InvGroup[2] = InvGroup.InvGroup.

lcExFile = "".
ufkey = TRUE.

toimi:
repeat WITH FRAME rajat ON ENDKEY UNDO toimi, NEXT toimi:

      DISPLAY 
      date1 date2 
      liCust-nr1 liCust-nr2
      ldtUnbilled
      llMthSplit
      xBilled
      xUnbilled
      xPeriodized
      llCalls
      llOldCalls
      llFees
      xIgSplit
      InvGroup
      llPaper
      llExcel lcExFile
      llSap   lcSapFile
      lcFileDir
      WITH FRAME rajat.

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0 /* 847 */
         ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      READKEY.
      nap = keylabel(LASTKEY).

      if lookup(nap,"1,f1") > 0 THEN DO:
         ehto = 9. RUN Syst/ufkey.p.
         ufkey = TRUE. 
         UPDATE 
                date1
                date2  
                    validate(INPUT date2 >= INPUT date1, 
                             "Invalid choice")
                InvGroup[1] 
                InvGroup[2] 
                    validate(INPUT InvGroup[2] >= INPUT InvGroup[1],
                             "Invalid choice")
                xIgSplit
                liCust-nr1 
                liCust-nr2
                xBilled
                xUnbilled
                ldtUnbilled
                llMthSplit
                xPeriodized
                llCalls
                llOldCalls
                llFees 
                llPaper
                llExcel lcExFile
                llSap   lcSapFile
                WITH FRAME rajat.
                
         IF NOT llCalls THEN llOldCalls = FALSE.
           
         NEXT toimi.
      END.

      else if lookup(nap,"5,f5") > 0 THEN DO:
      
         IF (llExcel AND lcExFile = "") OR
            (llSap   AND lcSapFile = "") 
         THEN DO:
            MESSAGE "File names have not been given"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         IF NOT llPaper AND
            NOT llExcel AND
            NOT llSap 
         THEN DO:
            MESSAGE "No output format has been chosen"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END. 
            
         LEAVE toimi.
      END.

      else if lookup(nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.

END. /* toimi */

/* Avataan striimi */
IF llPaper THEN DO:
    ASSIGN tila = TRUE.
    {Syst/tmsreport.i "return"}
END.

ehto = 5.
RUN Syst/ufkey.p.

/* info line for log */
lcLogLine = STRING(date1,"999999") + "-" +
            STRING(date2,"999999") + ":" +
            InvGroup[1] + "-" +
            InvGroup[2] + ":" +
            STRING(xBilled,"B/*") +
            STRING(xUnbilled,"U/*") + 
            STRING(xPeriodized,"P/*").

            
fELog("REVENUE","Started:" + lcLogLine).
 
ASSIGN lcExFile     = lcFileDir + "/" + lcExFile
       lcSapFile    = lcFileDir + "/" + lcSapFile
       ldtStartDate = TODAY
       ldBegTime    = fMakeTS()
       lcStartTime  = STRING(TIME,"hh:mm:ss").
       
DISPLAY ldtStartDate lcStartTime WITH FRAME rajat.        
       
RUN Ar/accdatli.p  (date1,
               date2,
               liCust-nr1,
               liCust-nr2,
               InvGroup[1],
               InvGroup[2],
               xIgSplit,
               xBilled,
               xUnbilled,
               ldtUnbilled,
               llMthSplit,
               llCalls,
               llOldCalls,
               llFees,
               xPeriodized,
               llPaper,
               llExcel,
               lcExFile,
               llSap,
               lcSapFile).

fELog("REVENUE","Stopped:" + lcLogLine).

IF llPaper THEN DO:
    ASSIGN tila = FALSE.
    {Syst/tmsreport.i}
END.

ldEndTime = fMakeTS().

/* duration */
liDurDays = DYNAMIC-FUNCTION("fTSDuration" IN ghfunc1,
                             ldBegTime,
                             ldEndTime,
                             OUTPUT liDurTime).
 
MESSAGE "Revenue report finished." SKIP
        "Duration was" 
        (IF liDurDays > 0 
         THEN STRING(liDurDays) + " days and"
         ELSE "")
        STRING(liDurTime,"hh:mm:ss") SKIP
VIEW-AS ALERT-BOX TITLE " DONE ". 

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.


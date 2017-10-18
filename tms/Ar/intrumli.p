/* ===========================================================================
 MODULE ........: intrumli.p
 APPLICATION ...: TMS
 TASK ..........: Print a File WITH overdue invoices TO Intrum Justitia
 CREATED .......: 22.03.01/aam 
 CHANGED .......: 27.02.02 ht  JG FIN Version
                  13.03.02/aam optionally count late days from reminding DAY
 CHANGED .......: 10.04.02/ht  moving the File into Transfer directory
                  18.09.02/jp  some help text
                  12.09.03/aam brand
                  02.06.04/aam delete file if no invoices found
                  26.08.04/aam count days from 2. reminder 
                  28.01.05/aam ldMinDebt, minimum limit for total debt
                  28.04.05/aam customer count from intrumex
                  29.12.05/aam write to eventlog (external)
                  27.06.06/aam limit fields removed
 Version .......: M15
 ============================================================================*/

{Syst/commali.i}                      
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/eventlog.i}

DEF VAR ok        AS LO  NO-UNDO FORMAT "Yes/No".
DEF VAR exPaymFile AS C   NO-UNDO FORMAT "x(50)".
DEF VAR xCount    AS I   NO-UNDO.
DEF VAR xDate1    AS DA  NO-UNDO.
DEF VAR xDate2    AS DA  NO-UNDO.
DEF VAR xCustNum1 AS INT NO-UNDO.
DEF VAR xCustNum2 AS INT NO-UNDO.
DEF VAR xDue      AS INT NO-UNDO.
DEF VAR xClaimed  AS INT NO-UNDO.
DEF VAR xBatch    AS INT NO-UNDO.
DEF VAR xMinClAmt AS DEC NO-UNDO.
DEF VAR xOk       AS LO  NO-UNDO.
DEF VAR LCfile    AS C   NO-UNDO FORMAT "x(45)".
DEF VAR LLchanged AS LO  NO-UNDO.
DEF VAR iBatch    AS INT NO-UNDO.
DEF VAR yBatch    AS INT NO-UNDO.
DEF VAR xLen      AS INT NO-UNDO.
DEF VAR xFromRem  AS LOG NO-UNDO. 

DEF VAR xTransDir    AS CHAR  NO-UNDO.
DEF VAR xFileExt     AS CHAR  NO-UNDO.
DEF VAR liLateDays   AS INT   NO-UNDO.
DEF VAR ldMinDebt    AS DEC   NO-UNDO.
DEF VAR liClaimPoint AS INT   NO-UNDO. 
DEF VAR liCustCnt    AS INT   NO-UNDO. 

DEF BUFFER bTMSParam FOR TMSParam.

ASSIGN
   xTransDir    = fCParamC("IntrumTransDir")
   xMinClAmt    = fCParamDe("MinClaimAmt")
   ldMinDebt    = fCParamDe("MinClaimTotDebt")
   iBatch       = fCParamI("IntrumFileSeqNbr") + 1
   lcFile       = fCParamC("IntrumCollFile")
   liLateDays   = fCParamI("ClaimLateDays")
   liClaimPoint = fCParamI("ClaimPoint")
   exPaymFile   = lcFile.


form 
skip(2)
"Program writes out an ASCII file with overdue invoices," AT 10
SKIP
"using Intrum Justitia's File description." AT 10 
skip(1)
xCustNum1  AT 10 LABEL "Customers .." FORMAT "zzzzzzz9" 
    HELP "Agreement customers"
"-"
xCustNum2    NO-LABEL FORMAT "zzzzzzzz9" 
    HELP "Agreement customers"
    VALIDATE(INPUT xCustNum2 GE INPUT xCustNum1,
             "Check customer definition") SKIP

ldMinDebt AT 10  LABEL "Minimum Debt" FORMAT ">>>>>9.99"
    HELP "Minimum limit for customer's total debt"
    
SKIP(1)
exPaymFile AT 10 LABEL "File Name .." 
    help "Name of Output File"
SKIP(8)
 WITH  OVERLAY ROW 1 WIDTH 80
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
       " " + ynimi + " SEND INVOICES TO COLLECTION " + 
       STRING(pvm,"99-99-99") + " " 
    SIDE-LABELS 
    /*1 columns*/
    FRAME main.

ASSIGN xDate2    = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       xDate1    = DATE(MONTH(xDate2),1,YEAR(xDate2) - 1)
       xCustNum2 = 999999999
       xDue      = liLateDays
       xFromRem  = FALSE
       xClaimed  = liClaimPoint - 1.

PAUSE 0.
DISPLAY ldMinDebt WITH FRAME main.

MAIN:
REPEAT WITH FRAME main:

    ehto = 9. RUN Syst/ufkey.p.

    UPDATE
    xCustNum1 xCustNum2
    exPaymFile
    WITH FRAME main.

    Action:
    REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = (IF exPaymFile ne "" THEN 795 ELSE 0).
      ufk[8] = 8.
      RUN Syst/ufkey.p.

      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:

         ok = FALSE.
         MESSAGE "Do You REALLY want to start (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.

         LEAVE Action.
      END.
   END. /* Action */      

   Message "Processing...".

   /* Adds sequence number into File Name */
   ASSIGN
      xLen     = INDEX(exPaymFile,".") - 1
      xFileExt = SUBSTR(exPaymFile,xLen + 1).

   exPaymFile = SUBSTR(exPaymFile,1,xLen) + 
            STRING((IF xBatch = 0 THEN iBatch ELSE xBatch),"999")  +
            xFileExt.

   fELog("CLAIM","Started").

   RUN intrumex   (xCustNum1,
                   xCustNum2,
                   xBatch,
                   (IF xBatch = 0 THEN iBatch ELSE xBatch),
                   ldMinDebt,
                   exPaymFile,
                   OUTPUT xCount,
                   OUTPUT liCustCnt,
                   OUTPUT yBatch).

   fELog("CLAIM","Stopped:" + STRING(xCount)).

   /* UPDATE Seq nbr */
   IF xBatch = 0 AND xCount NE 0 THEN DO FOR bTMSParam TRANS:
      FIND bTMSParam WHERE 
           bTMSParam.Brand     = gcBrand AND
           bTMSParam.ParamCode = "IntrumFileSeqNbr" NO-ERROR.
      ASSIGN  btmsparam.intVal = yBatch.
      RELEASE bTMSParam.
   END.

   PAUSE 0.
   IF xCount > 0 THEN 
   MESSAGE 
     xCount "invoices for" 
     liCustCnt "customers were printed as batch nbr" 
     yBatch "into file"    SKIP
     exPaymFile            skip(1)
   VIEW-AS ALERT-BOX. 
   ELSE MESSAGE "No invoices were found to be claimed"
        VIEW-AS ALERT-BOX.

   /* Move the NEW File TO the actual Transfer directory */
   IF xCount NE 0 THEN DO:
      fTransDir(exPaymFile,
                xFileExt,
                xTransDir).
   END.
   ELSE DO:
      OS-DELETE VALUE(exPaymFile).
   END.

   LEAVE main.

END. /* MAIN */             

HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.


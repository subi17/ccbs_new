/* ------------------------------------------------------
  MODULE .......: INTRUMCRP
  FUNCTION .....: List error list from booking intrum cancel file
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 19.11.02
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/utumaa.i}
{Ar/intrumcr.i}

DEF INPUT PARAMETER TABLE FOR ttError.

DEF INPUT PARAMETER icFile AS CHAR NO-UNDO.

DEF VAR lcLine1      AS CHAR NO-UNDO FORMAT "x(112)".
DEF VAR lcLine2      LIKE lcLine1.
DEF VAR lcLine3      LIKE lcLine1.
DEF VAR lcLine4      LIKE lcLine1.
DEF VAR liPage       AS INT  NO-UNDO.
DEF VAR liLine       AS INT  NO-UNDO.
DEF VAR liWidth      AS INT  NO-UNDO INIT 112.
DEF VAR lcError      AS CHAR NO-UNDO.
DEF VAR liCount      AS INT  NO-UNDO. 

ASSIGN 
    lcLine1   = FILL("=",liWidth)
    lcLine2   = FILL("=",liWidth)
    lcLine3   = FILL("-",liWidth)
    lcLine4   = FILL("-",liWidth).

form header
   lcLine1 AT 1 SKIP
   ynimi  AT 1 FORMAT "x(30)" 
      "ERRORS IN INTRUM CREDIT LOSS FILE" AT 40
      "Page" AT 103
      liPage FORMAT "ZZZZ9" SKIP
   icFile AT 1 FORMAT "X(50)"
      pvm FORMAT "99.99.99" AT 105 SKIP
   lcLine2 AT 1 SKIP

   "Invoice"      AT 1
   "Customer"     AT 22 
   "Org/Soc."     AT 38
   "Name"         AT 49
   "Int.Ref"      AT 85
   "C"            TO 94
   "Amount"       TO 106
   "Error"        TO 112 
   SKIP

   lcLine3 AT 1 SKIP
   WITH WIDTH 112 NO-LABEL NO-BOX FRAME fPageHead.


FUNCTION fChkPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF liLine + iAddLine >= skayt1 THEN DO:

        IF liPage > 0 THEN DO:
           {Syst/uprfeed.i liLine}
        END.

        liPage = liPage + 1.
        VIEW STREAM tul FRAME fPageHead.
        liLine = 6.

        /* errorcodes to the first page */
        IF liPage = 1 THEN 
        DO liCount = 1 TO NUM-ENTRIES(lcErrorExpl):

           IF liCount = 1 
           THEN PUT STREAM tul UNFORMATTED "Errorcodes:" AT 1.

           PUT STREAM tul UNFORMATTED
              liCount AT 15 
              " = "
              ENTRY(liCount,lcErrorExpl)
              SKIP.
           liLine = liLine + 1.
        END. 

        RETURN TRUE.
    END.    

    ELSE RETURN FALSE.
END.

fChkPage(999). 

FOR EACH ttError:

   fChkPage(0).

   PUT STREAM tul 
      ttError.Inv       AT 1   FORMAT "X(20)"
      ttError.Cust      AT 22  FORMAT "X(15)"
      ttError.Org       AT 38  FORMAT "X(10)"
      ttError.Name      AT 49  FORMAT "X(35)"
      ttError.IntrRef   AT 85  FORMAT "X(7)"
      ttError.ClCancel  TO 94  FORMAT ">9"
      ttError.Amount    TO 106 FORMAT "->>>>>>9.99"
      ttError.ErrCode   TO 112 FORMAT ">>9"
      SKIP.

   ASSIGN liLine = liLine + 1.
END.



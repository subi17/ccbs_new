 /* ------------------------------------------------------
  MODULE .......: coacrel.p
  FUNCTION .....: ui for commission payment report
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 30.06.05/aam (from copayrel) 
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/utumaa.i "new"}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'coevent'}

assign tuni1 = "coacrel"
       tuni2 = "".


DEF VAR lcUfkey      AS LOG                     NO-UNDO.
DEF VAR liCoRuleID   AS INT                     NO-UNDO. 
DEF VAR lcFile       AS CHAR  FORMAT "X(40)"    NO-UNDO.
DEF VAR lcReseller   AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2.
DEF VAR lcSalesman   AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2. 
DEF VAR ldtCalcDate  AS DATE  FORMAT "99-99-99" NO-UNDO EXTENT 2.
DEF VAR ldtOldPDate  AS DATE  FORMAT "99-99-99" NO-UNDO EXTENT 2.
DEF VAR liCustNum    AS INT   FORMAT ">>>>>>>9" NO-UNDO EXTENT 2.
DEF VAR llMark       AS LOGIC FORMAT "Yes/No"   NO-UNDO. 
DEF VAR liCount      AS INT                     NO-UNDO. 
DEF VAR ldtPaymDate  AS DATE  FORMAT "99-99-99" NO-UNDO. 

DEF TEMP-TABLE ttMark NO-UNDO
   FIELD EventID AS INT. 

FORM 
   SKIP(2)
   "This program prints out a report of commission for AC-resellers." 
      AT 10 SKIP
   "Events can be marked as paid after report is finished." AT 10 
   SKIP(2)

   liCoRuleID AT 10 
        LABEL "Rule ID ....."
        HELP "Events of one rule, or all = 0 (empty)"
        VALIDATE(INPUT liCoRuleID = 0 OR 
                 CAN-FIND(CoRule WHERE 
                          CoRule.Brand    = gcBrand AND
                          CORule.CoRuleID = INPUT liCoRuleID),
                 "Unknown rule")
        FORMAT ">>>>>>>>"
        SKIP

   lcReseller[1] AT 10
        LABEL "Resellers ..."
        HELP  "Resellers"
   "-"
   lcReseller[2]
        NO-LABEL
        HELP "Resellers"
        VALIDATE(INPUT lcReseller[2] >= INPUT lcReseller[1],
                 "Invalid definition")
        SKIP

   lcSalesman[1] AT 10
        LABEL "Salesmen ...."
        HELP  "Salesmen"
   "-"
   lcSalesman[2]
        NO-LABEL
        HELP "Salesmen"
        VALIDATE(INPUT lcSalesman[2] >= INPUT lcSalesman[1],
                 "Invalid definition")
        SKIP

   liCustNum[1] AT 10
        LABEL "Customers ..."
        HELP  "Customers"
   "-"
   liCustNum[2]
        NO-LABEL
        HELP "Customers"
        VALIDATE(INPUT liCustNum[2] >= INPUT liCustNum[1],
                 "Invalid definition")
        SKIP                 

   ldtCalcDate[1] AT 10
        LABEL "Calcul. Dates"
        HELP  "Event calculation (creation) dates"
   "-"
   ldtCalcDate[2]
        NO-LABEL
        HELP  "Event calculation (creation) dates"
        VALIDATE(INPUT ldtCalcDate[2] >= INPUT ldtCalcDate[1],
                 "Invalid definition")
        SKIP                 

   ldtOldPDate[1] AT 10
        LABEL "Payment Dates"
        HELP  "Payment dates for previously paid events (empty = unpaid)"
   "-"
   ldtOldPDate[2]
        NO-LABEL
        HELP  "Payment dates for previously paid events (empty = unpaid)"
        VALIDATE(INPUT ldtOldPDate[2] >= INPUT ldtOldPDate[1],
                 "Invalid definition")

   SKIP(1)
   ldtPaymDate AT 10 
        LABEL "Mark Date ..." 
        HELP  "Mark as payment date for listed events (empty = leave unpaid)"
   lcFile AT 10
        LABEL "File ........"
        HELP  "Name for the tab separated file"
   SKIP(2)

   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " COMMISSION REPORT " +
        STRING(pvm,"99-99-99") + " "
        FRAME valinta.

VIEW FRAME valinta.
PAUSE 0 NO-MESSAGE.

FOR EACH Reseller NO-LOCK WHERE 
         Reseller.Brand = gcBrand AND
         Reseller.Reseller BEGINS "AC"
BY Reseller.Reseller:

    IF lcReseller[1] = "" THEN lcReseller[1] = Reseller.Reseller.
    lcReseller[2] = Reseller.Reseller.
    
    FIND FIRST Salesman NO-LOCK WHERE
               Salesman.Brand    = gcBrand AND
               Salesman.Reseller = Reseller.Reseller AND
               Salesman.RsLevel  = 1 NO-ERROR.
    IF AVAILABLE Salesman THEN DO:
             
       IF lcSalesman[1] = "" THEN lcSalesman[1] = Salesman.Salesman.
                
       lcSalesman[1] = MIN(lcSalesman[1],Salesman.Salesman).
    END. 
       
    FIND LAST Salesman NO-LOCK WHERE
              Salesman.Brand    = gcBrand AND
              Salesman.Reseller = Reseller.Reseller AND
              Salesman.RsLevel  = 1 NO-ERROR.
    IF AVAILABLE Salesman THEN 
    lcSalesman[2] = MAX(lcSalesman[2],Salesman.Salesman).
END.
         
ASSIGN liCustNum[2]   = 99999999          
       ldtCalcDate[1] = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldtCalcDate[2] = IF MONTH(TODAY) = 12 
                        THEN DATE(12,31,YEAR(TODAY))
                        ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1.

ASSIGN lcUfkey = FALSE
       nap     = "first". 

toimi:
REPEAT WITH FRAME valinta on ENDkey undo toimi, NEXT toimi:

   PAUSE 0.
   DISPLAY liCORuleID
           lcReseller
           lcSalesman
           liCustNum
           ldtCalcDate
           ldtOldPDate
           ldtPaymDate
           lcFile
   WITH FRAME valinta. 
   
   if lcUfkey THEN DO:

      ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0 
         ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 
         lcUfkey = FALSE.

      RUN ufkey.

   END.

   IF nap NE "first" THEN DO:
      READKEY.
      nap = KEYLABEL(LASTKEY).
   END.
   ELSE ASSIGN nap = "1". 

   IF LOOKUP(nap,"1,f1") > 0 THEN DO:

      ehto = 9. 
      RUN ufkey.p.

      REPEAT WITH FRAME valinta ON ENDKEY UNDO, LEAVE:

         UPDATE liCORuleID
                lcReseller
                lcSalesman
                liCustNum
                ldtCalcDate
                ldtOldPDate
                ldtPaymDate
                lcFile
         WITH FRAME valinta EDITING:

            READKEY.

            IF LOOKUP(keylabel(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME valinta:

               HIDE MESSAGE.

               IF FRAME-FIELD = "liCustNum" THEN DO :
               END.

            END.

            APPLY LASTKEY.

         END.

         LEAVE. 

      END.

      IF ldtOldPDate[2] = ? THEN ldtOldPDate[1] = ?.

      lcUfkey = TRUE.

      NEXT toimi.
   END.

   ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:
      IF lcFile = "" THEN DO:
         MESSAGE "File name has not been given"
         VIEW-AS ALERT-BOX ERROR.
         NEXT. 
      END. 
      
      LEAVE toimi.
   END.

   ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
      RETURN.
   END.

END. /* toimi */

ehto = 5.
RUN ufkey.

IF lcFile = "" THEN DO:
   assign tila = true.
   {Syst/utuloste.i "return"}
END.

RUN coacrep   (liCORuleID,
               lcReseller[1],
               lcReseller[2],
               lcSalesman[1],
               lcSalesman[2],
               liCustNum[1],
               liCustNum[2],
               ldtCalcDate[1],
               ldtCalcDate[2],
               ldtOldPDate[1],
               ldtOldPDate[2],
               lcFile,
               OUTPUT TABLE ttMark,
               OUTPUT liCount).

IF lcFile = "" THEN DO:
   assign tila = false.
   {Syst/utuloste.i}
END.

llMark = FALSE.
IF ldtPaymDate NE ? THEN 
   MESSAGE liCount "events were printed on commission report." SKIP
           "Shall listed events be marked as paid ?"
   VIEW-AS ALERT-BOX
   QUESTION
   BUTTONS YES-NO
   SET llMark.
ELSE 
   MESSAGE liCount "events were printed on commission report." 
   VIEW-AS ALERT-BOX
   TITLE " Finished ".

IF llMark THEN DO:
   RUN copayrem (INPUT TABLE ttMark,
                 INPUT  ldtPaymDate,
                 OUTPUT liCount).

   MESSAGE liCount "events were marked as paid."
   VIEW-AS ALERT-BOX
   INFORMATION.
END.

HIDE MESSAGE NO-PAUSE.
HIDE FRAME valinta NO-PAUSE.    


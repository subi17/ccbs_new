/* ===========================================================================
 MODULE ........: moveSIM.p
 TASK ...... ...: Move SIM cards from on Stock into another
 APPLICATION ...: Ticket Master
 CREATED .......: 29.06.99 pt
 CHANGED .......: 04.08.99 pt enter '+n' @ icc2
                  06.11.02 jr Eventlog
                  11.09.03 jp Brand
 Version .......: M15
 ============================================================================*/

{commali.i}
{utumaa.i new}
{eventval.i} 

ASSIGN tuni1 = "*******"
       tuni2 = "".

DEF STREAM iccfile.

DEF BUFFER Stock2 FOR Stock.       

DEF VAR ok       AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR Stock1 LIKE Stock.Stock NO-UNDO.
DEF VAR Stock2 LIKE Stock.Stock NO-UNDO.
DEF VAR Qty      AS i  NO-UNDO.
DEF VAR SIMStat  LIKE SIMStat.SIMStat NO-UNDO.
DEF VAR icc1     LIKE SIM.ICC         NO-UNDO.
DEF VAR icc2     LIKE SIM.ICC         NO-UNDO.
DEF VAR SimArt  LIKE SimArt.SimArt  NO-UNDO.
DEF VAR i        AS i                 NO-UNDO.
DEF VAR make-iccfile AS lo            NO-UNDO.
DEF VAR tab      AS C                 NO-UNDO.   tab = chr(9).
DEF VAR mess     AS C                 NO-UNDO.

DEF BUFFER xSIM FOR SIM.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhStoBal AS HANDLE NO-UNDO.
   lhStoBal = BUFFER StoBal:HANDLE.
   RUN StarEventInitialize(lhStoBal).

   DEFINE VARIABLE lhSim AS HANDLE NO-UNDO.
   lhSim = BUFFER Sim:HANDLE.
   RUN StarEventInitialize(lhSim).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhSim).
   END.
END.

FORM
   skip(1)
"  Note:  You can now move a set of SIM cards from one Stock to" skip
"         another, according Your choices below:"                skip(1)
"         From Stock .:" Stock1 Stock.StoName   at 40 format "x(30)"        skip
"         To   Stock .:" Stock2 Stock2.StoName  at 40 format "x(30)"        skip
"         SIM status .:" SIMStat  SIMStat.SSName at 40 format "x(30)"        skip
"         SIM Article :" SimArt   SimArt.SAName at 40 format "x(30)"        skip
"         ICC Range...:" icc1 VALIDATE(input icc1 ne "","Missing ICC ID !")
help "Smallest ICC ID to be moved" "-" icc2
help "Largest ICC ID to be moved (or +n: totally n cards from icc-1)"
VALIDATE(input icc2 >= input icc1 or   (input icc2 begins "+" and input icc1 ne ""),
          "Invalid Order !")
   skip(2)
"         ICC InfoFile:" Stock2.StoFile1
skip(4)              
WITH
   width 80
   OVERLAY
   TITLE 
   " " + ynimi + 
   " MOVE SIM CARDS " +
   string(pvm,"99.99.9999") + " "
   NO-LABELS
   FRAME MAIN.


/* get default codes */
{tmsparam.i "MainStock"     return}. Stock1 = TMSParam.CharVal.
{tmsparam.i "DefDelivStock" return}. Stock2 = TMSParam.CharVal.
{tmsparam.i "SIMStatusNew"  return}. SIMStat  = TMSParam.IntVal.

PAUSE 0.

MAIN:
REPEAT WITH FRAME main:

   ehto = 9. RUN ufkey.

   UPDATE
   Stock1 Stock2 SIMStat SimArt icc1 icc2
   WITH FRAME main EDITING:
      READKEY.

      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME main:
         HIDE MESSAGE.

         IF FRAME-FIELD = "stock1" THEN DO:
            FIND Stock WHERE 
                 Stock.Brand = gcBrand   AND 
                 Stock.Stock = INPUT Stock1 NO-LOCK NO-ERROR.
            IF NOT AVAIL Stock THEN DO:
               BELL.
               MESSAGE "UNKNOWN Stock !".
               NEXT.
            END.
            DISPLAY Stock.StoName .
         END.

         ELSE IF FRAME-FIELD = "SimArt" THEN DO:
            FIND SimArt WHERE 
                 SimArt.Brand  = gcBrand AND 
                 SimArt.SimArt = INPUT SimArt NO-LOCK NO-ERROR.
            IF NOT AVAIL SimArt THEN DO:
               BELL.
               MESSAGE "UNKNOWN SIM ARTICLE !".
               NEXT.
            END.
            DISPLAY SimArt.SAName .
         END.
         ELSE IF FRAME-FIELD = "Stock2" THEN DO:
            FIND Stock2 WHERE 
                 Stock2.Brand  = gcBrand AND 
                 Stock2.Stock = INPUT Stock2 NO-LOCK NO-ERROR.
            IF NOT AVAIL Stock2 THEN DO:
               BELL.
               MESSAGE "UNKNOWN Stock !".
               NEXT.
            END.
            DISPLAY Stock2.StoName Stock2.StoFile1.
            make-iccfile = Stock2.StoFile1 ne "".

         END.

         ELSE IF FRAME-FIELD = "SIMStat" THEN DO:
            FIND SIMStat WHERE 
                 SIMStat.SIMStat = INPUT SIMStat NO-LOCK NO-ERROR.
            IF NOT AVAIL SIMStat THEN DO:
               BELL.
               MESSAGE "UNKNOWN STATUS CODE !".
               NEXT.
            END.
            DISPLAY SIMStat.SSName .
         END.

         ELSE IF FRAME-FIELD = "icc2" THEN DO:
            IF INPUT icc2 begins "+" THEN DO:
               Qty = integer(substr(INPUT icc2,2)).

               FIND xSIM WHERE 
                    xSim.Brand = gcBrand AND 
                    xSIM.ICC   = INPUT icc1 NO-LOCK.
               IF NOT AVAIL xSIM THEN DO:
                  BELL.
                  MESSAGE "Check 1st ICC code - it does not exist !".
                  NEXT.
               END.

               DO i = 1 TO Qty - 1.  
                  FIND NEXT xSIM WHERE
                            xSim.Brand = gcBrand 
                  no-lock no-error.
                  IF NOT AVAIL xSIM THEN DO:
                     BELL.
                     MESSAGE 
                     "Not enough SIM cards - only" SKIP
                     i - 1 "were found in this range"
                     VIEW-AS ALERT-BOX TITLE 
                     " OUT OF CARDS ".
                     LEAVE.
                  END.
                  ICC2 = xSIM.ICC.
               END.
               DISP icc2.
            END.

         END.

      END.      
      APPLY LASTKEY.
   END.   

ACTION:
   REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = 795
      ufk[8] = 8.

      IF length(icc1) NE length(icc2) THEN DO:
         MESSAGE 
         "Both ICC ID Codes must be of same length !"
         VIEW-AS ALERT-BOX ERROR TITLE " INVALID ICC ID CODE(S) ".
         ufk[5] = 0.
      END.   


      RUN ufkey.

      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:

         i = 0.
         FOR 
         EACH SIM no-lock where
              SIM.Stock = Stock1   AND
              SIM.SimArt = SimArt    AND
              SIM.ICC    >= icc1       AND
              SIM.ICC    <= icc2:

            i = i + 1.
         END.
         IF i = 0 THEN DO:
            MESSAGE 
            "There were NO SIM Cards in Stock" Stock1 SKIP
            "to match the given values !"
            VIEW-AS ALERT-BOX ERROR TITLE " NO SIM CARDS WERE FOUND ".
            NEXT Action.
         END.

         MESSAGE "Do You REALLY want to MOVE" i "SIM Cards (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.

         LEAVE Action.
      END.
   END. /* Action */      

   IF make-iccfile THEN OUTPUT STREAM iccfile TO VALUE(Stock2.StoFile1) APPEND.


   FOR EACH SIM EXCLUSIVE-LOCK WHERE
            SIM.Stock = Stock1   AND
            SIM.SimArt = SimArt  AND
            SIM.ICC    >= icc1   AND
            SIM.ICC    <= icc2   AND 
            SIM.Brand   = gcBrand :

       FIND  StoBal WHERE
             Stobal.Brand  = gcBrand    AND 
             StoBal.SimArt = SIM.SimArt AND
             StoBal.StoBal = SIM.Stock
       EXCLUSIVE-LOCK no-error.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhStoBal).
       /* subtract 1 from Stock Balance */
       ASSIGN 
          StoBal.Balance    = StoBal.Balance    - 1
          StoBal.DetBal[1] = StoBal.DetBal[1] - 1.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhStoBal).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSim).
       /* Move the SIM into another Stock */
       SIM.Stock = Stock2.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSim).

       /* Add 1 TO Stock Balance of receiving Stock */
       FIND  StoBal WHERE
             StoBal.SimArt = SIM.SimArt AND
             StoBal.StoBal = SIM.Stock  AND 
             StoBal.Brand  = gcBrand 
       EXCLUSIVE-LOCK NO-ERROR.
       IF NOT AVAIL StoBal THEN DO:
          CREATE StoBal.
          ASSIGN
             Stobal.Brand     = gcBrand 
             StoBal.SimArt    = SIM.SimArt
             StoBal.StoBal    = SIM.Stock
             StoBal.Balance   = StoBal.Balance    + 1
             StoBal.DetBal[1] = StoBal.DetBal[1] + 1.
          IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhStoBal).   
       END.
       ELSE 
       DO:
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhStoBal).
          ASSIGN
            StoBal.Balance   = StoBal.Balance    + 1
            StoBal.DetBal[1] = StoBal.DetBal[1] + 1.
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhStoBal).  
       END.
       IF make-iccfile THEN PUT STREAM iccfile UNFORMATTED
          "ADD ICC"  tab
          SIM.ICC    tab
          chr(13) chr(10).
   END.

   IF make-iccfile THEN DO:
      OUTPUT STREAM iccfile CLOSE.
      mess = "ICC INFO FILE: " + Stock2.StoFile1.
   END.
   ELSE  mess = "". 

   MESSAGE 
   "Totally" i "SIM Cards were moved " SKIP
   "FROM Stock" Stock1               SKIP
   "TO   Stock" Stock2               skip(1)
   mess
   VIEW-AS ALERT-BOX TITLE " CARDS ARE NOW MOVED ".

   LEAVE main.
END. /* MAIN */
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.

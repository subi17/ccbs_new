/*----------------------------------------------------------------------------
  MODULE .......: NNHIMU.P
  FUNCTION .....: Updates pricerates FOR countries
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 27.07.98 KL
  MODIFIED .....: 18.05.99 KL check valid from Date
                  06.11.99 KL FIND FIRST CCN
                  01.08.02 lp changed PAUSE 0. -> PAUSE. for disp tariff
                  11.09.02 jp "mn at 42" moved after help text
                  22.10.02 aam Customer and General removed
                               note: this should be changed so that new 
                               tariff is created and validto for old updated  
                  04.11.02 jr Eventlog
                  27.02.03 tk tokens
                  18.03.03 tk instruction note changed
                  15.09.03 aam brand
  Version ......: M15
 -------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'tariff'}

DEF VAR i       AS i  NO-UNDO.
def var ok      as lo no-undo format "Yes/No".
def var gp      as lo no-undo format "Yes/No".
def var ch      as lo no-undo format "Update/Display".
def var pr      as de no-undo format "-z9.99".
def var tz      as lo no-undo format "Yes/No" EXTENT 6 init TRUE.
def var an      as c  no-undo format "x(15)".
def var mn      as c  no-undo format "x(15)".
def var pn      as c  no-undo format "x(15)".
def var new-pr  as de no-undo format ">9.99999".
def var perce   as de no-undo format ">9.99999".

DEF VAR vdate   LIKE Tariff.ValidFrom    NO-UNDO.
DEF VAR PriceList LIKE Tariff.PriceList  NO-UNDO.
DEF VAR CustNum     LIKE Tariff.CustNum      NO-UNDO.
DEF VAR CCN  LIKE CCN.CCN    NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTariff AS HANDLE NO-UNDO.
   lhTariff = BUFFER Tariff:HANDLE.
   RUN StarEventInitialize(lhTariff).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhTariff).
   END.
END.

form
   skip(3)
   "  INSTRUCTION: This module updates rates for CCNs determined below."
   "               RATES WILL NOT BE UPDATED IF YOU CHOOSE DISPLAY !"  skip(1)
   "               Price list .....:" PriceList 
      help "Price list code, empty for all"                    
                   pn at 43  SKIP
   "               CCN ............:" CCN 
      help "CCN"   mn AT 43  SKIP
   "               Valid from .....:" vdate
      help "What is the VALID FROM Date of the Price ?"        SKIP   
   "               Percentage .....:" pr      
      help "Percentage of rate change, + to raise, - to lower" SKIP
   "               Time zones .....:"                                
   "1:  2:  3:  4:  5:  6:"                                   SKIP
   tz[1] AT 34      
   tz[2 FOR 5]                                                 skip(1)
   "               Update prices ..:" ch
      help "Update or just display new rates"                  
      skip(3)
WITH COLOR value(cfc)
   title color value(ctc) " " + ynimi + " Update rates for CCNs "
   OVERLAY width 80 ROW 1 NO-LABELS centered
   FRAME Limit.

form
   Tariff.BDest         label "B-number"
   Tariff.PriceList     label "PL"
   Tariff.Price[1]      label "Old price"
   new-pr               label "New price"
WITH ROW 3 centered OVERLAY FRAME frm 11 DOWN.


Limit:
repeat WITH FRAME Limit:
   ehto = 9. RUN Syst/ufkey.

   UPDATE
      PriceList
      CCN 
      vdate
      pr
      tz [1 FOR 6]
      ch
   WITH FRAME Limit EDITING:
      READKEY. nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:

         if frame-field = "CCN" THEN DO:
            ASSIGN CCN.
            IF CCN = 0 THEN DO:
               message "You can't change rates for every CCN"
                       " - press ENTER to continue !".
               PAUSE no-message.
               NEXT-PROMPT CCN.
               NEXT.
             END.
             ELSE DO:
                FIND FIRST CCN where 
                           CCN.Brand = gcBrand AND
                           CCN.CCN   = CCN 
                no-lock no-error.
                IF AVAIL CCN THEN 
                   DISP CCN.CCNName @ mn.
                ELSE DO:
                   message "Unknown CCN - press ENTER to continue !".
                   PAUSE no-message.
                   NEXT-PROMPT CCN.
                   NEXT.
                END.
             END.
         END.

         else if frame-field = "PriceList" THEN DO:
            ASSIGN PriceList.
            if PriceList = "" then disp "ALL" @ pn.
            ELSE DO:
               FIND PriceList where     
                    PriceList.Brand     = gcBrand AND
                    PriceList.PriceList = PriceList
               no-lock no-error.
               IF AVAIL PriceList THEN 
                  DISP PriceList.PLName @ pn.
               ELSE DO:
                  message "Unknown Price List - press ENTER to continue !".
                  PAUSE no-message.
                  NEXT-PROMPT PriceList.
                  NEXT.
               END.
            END.
         END.
         ELSE IF FRAME-FIELD = "ch" THEN DO:
            IF lcRight NE "RW" and INPUT ch = TRUE THEN DO:
               BELL.
               MESSAGE "You don't have right to update !".
               NEXT.

            END.

         END.
      END.
      APPLY LASTKEY.
   END.

toimi:
   repeat WITH FRAME Limit:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 7 ufk[5] = 795 ufk[8] = 8.
      RUN Syst/ufkey.
      IF toimi = 1 THEN NEXT  Limit.
      IF toimi = 8 THEN LEAVE Limit.
      IF toimi = 5 THEN DO:
         BELL.
         ok = FALSE.
         message "Are You sure You want to start counting ? (Y/N) " UPDATE ok.
         IF ok THEN LEAVE toimi.
      END.
   END.  /* toimi */

   IF pr > 0 THEN perce = 1 + (pr / 100).
   ELSE IF pr < 0 THEN perce = 1 + (-1 * pr / 100).

   FOR EACH Tariff exclusive-lock where
            Tariff.Brand  = gcBrand  AND
            Tariff.CCN    = CCN      AND 
           /* only one Date AT a time */
            Tariff.ValidFrom = vdate AND
           /* ALL OR selected Price list ? */
           (if PriceList ne "" THEN
            Tariff.PriceList = PriceList
            ELSE TRUE)       AND

      ok = FALSE.

      IF ok THEN DO:

         IF pr NE 0 THEN new-pr = Tariff.Price[1] +
                                 (Tariff.Price[1] * pr / 100).
         ELSE new-pr = Tariff.Price[1].

         DISP 
            Tariff.BDest 
            Tariff.PriceList
            Tariff.Price[1] 
            new-pr
         WITH NO-LABELS FRAME frm. 
         DOWN WITH FRAME frm.
         PAUSE.

         IF ch THEN DO:
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTariff).
            /* time zones */
            DO i = 1 TO 6:
               IF tz[i] AND Tariff.Price[i] NE 0 THEN ASSIGN
                  Tariff.Price[i] = Tariff.Price[i] + 
                                   (Tariff.Price[i] * pr / 100).
            END.
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTariff).
         END. /* IF ch */

      END. /* IF ok */

   END. 
   HIDE FRAME frm no-pause.

END. /* Limit */
HIDE FRAME Limit no-pause.

/*-----------------------------------------------------------------------------
  MODULE .......: AGEANAL.P
  FUNCTION .....: Myyntisaamisten ikAjakauman print-line
  SOVELLUTUS ...: VP
  AUTHOR .......: TT
  CREATED ......: 09.07.1991
  changePVM ....: 14.08.1991/tt
                  23.11.1997/pt  ig-code -rajaus
                  08.07.1998/kl  english
                  04.06.1999/kl  day-to
                  09.11.2000/ht  eiera- and asaldo-values to 10 millions
                  11.02.02 lp    changed swedish to english
                  08.03.02/aam   longer format for CustNum in by-sort 
                  17.06.2002/aam skip invoices defined in parameter 
                                 InvTypeDenied
                  02.09.2002/aam use invbal.p 
                  13.01.2002/jp  External customer group
                  20.05.2003/aam use finvbal
                  20.08.2003/aam separated from nnikaj.p; 
                                 2 additional day columns,
                                 optionally to Excel,
                                 optionally only summary,
                                 use ttInvGroup etc.
                  19.09.2003/aam brand                                        
                  27.01.2005/aam don't use "invoice of customer"
                  07.03.2005/aam use RepDate as situation date
                  10.08.06/aam   use fCalcInvBal instead of fInvBal
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/utumaa.i}
{Func/finvbal.i}

{Ar/ageanal.i}

DEF INPUT PARAMETER TABLE FOR TCustGroup.
DEF INPUT PARAMETER TABLE FOR ttCriter.

def var viiva1 as char format "x(133)" no-undo.
def var viiva2 like viiva1.
def var viiva3 like viiva1.
def var sl as int no-undo.
def var rl as int no-undo.
def var rlx as int no-undo.
def var lev as int init 133 no-undo.

def var asaldo as dec format "->>>>>>>9.99" init 0 no-undo.
def var ldInvBal like asaldo.
def var eiera  as dec format "->>>>>>>9.99" init 0 no-undo.
def var era1   like eiera.
def var era2   like eiera.
def var era3   like eiera.
def var era4   like eiera.
def var era5   like eiera.
def var era6   like eiera.
def var yasaldo as dec format "->>>>>>>9.99" init 0 no-undo.
def var yeiera  as dec format "->>>>>>>9.99" init 0 no-undo.
def var yera1   like yeiera.
def var yera2   like yeiera.
def var yera3   like yeiera.
def var yera4   like yeiera.
def var yera5   like yeiera.
def var yera6   like yeiera.

def var ryhma2 as char format "x(8)" no-undo.
def var ryhma3 as char format "x(8)" no-undo.
def var ryhma4 as char format "x(8)" no-undo.
def var ryhma5 as char format "x(8)" no-undo.
def var ryhma6 as char format "x(8)" no-undo.
def var ryhma7 as char format "x(8)" no-undo.

DEF VAR CgCustno1 AS INT no-undo.
DEF VAR cgCustno2 AS INT no-undo.

DEF VAR ExtCustGrp   AS LOG   NO-UNDO init FALSE FORMAT "X/-".
DEF VAR lcGrpHeader  AS CHAR  NO-UNDO. 
DEF VAR lcTypeDenied AS CHAR  NO-UNDO. 
DEF VAR lcSessionNum AS CHAR  NO-UNDO. 
DEF VAR liColl       AS INT   NO-UNDO. 

DEF TEMP-TABLE TCGMember NO-UNDO
   FIELD custno LIKE Customer.CustNum
   INDEX custno custno.

DEF TEMP-TABLE ttInvGroup NO-UNDO
   FIELD InvGroup AS CHAR.

DEF STREAM sLog.

assign
viiva1       = fill("=",lev)
viiva2       = fill("=",lev)
viiva3       = fill("-",lev)
lcTypeDenied = fCParamC("InvTypeDenied"). 


form header
   viiva1 at 1 skip
   ynimi  at 1 
      "AGE ANALYSIS" at 55 
      "Page" at 125 sl format "ZZ9" skip
   "Inv.Group:" at 2 lcGrpHeader format "x(24)"
      jar at 55 
      string(pvm,"99-99-99") at 125 skip
   viiva2 at 1 skip(1)

   "Cust."      to 8
   "Customer"   at 10
   "Debt"       to 42
   "Not"        to 55
   "Overdue"    to 68
   "Overdue"    to 81
   "Overdue"    to 94
   "Overdue"    to 107 
   "Overdue"    to 120 
   "Overdue"    to 133 
   skip

   "number"     to 8
   "name"       at 10
   "total"      to 42
   "overdue"    to 55
   ryhma2       to 68
   ryhma3       to 81
   ryhma4       to 94
   ryhma5       to 107
   ryhma6       to 120
   ryhma7       to 133
   skip

   viiva3 at 1 skip
   with width 133 no-label no-box frame sivuots.

form
   Customer.CustNum  to 8   format ">>>>>>>9"
   Customer.CustName at 10  format "x(20)"
   asaldo          to 42
   eiera           to 55
   era1            to 68
   era2            to 81
   era3            to 94
   era4            to 107 
   era5            to 120 
   era6            to 133 
   skip
   with width 133 no-labels no-box frame avline.

FUNCTION fChgPage RETURNS LOGICAL
   (iiAddLine AS INT).

   IF ttCriter.ToFile > "" THEN RETURN FALSE.

   if rl + iiAddLine >= skayt1 then do:

      if sl > 0 then do:
         {Syst/uprfeed.i rl}
      end.

      assign
         rlx = 0
         sl = sl + 1
         rl = 8.
      view stream tul frame sivuots.
   end.

END FUNCTION.

assign
   sl = 0
   rl = skayt1.

FIND FIRST ttCriter NO-ERROR.
IF NOT AVAILABLE ttCriter THEN RETURN.

/* output to file */
IF ttCriter.ToFile > "" THEN DO:
   ASSIGN lcSessionNum           = SESSION:NUMERIC-FORMAT
          SESSION:NUMERIC-FORMAT = "european".

   OUTPUT STREAM tul TO VALUE(ttCriter.ToFile).
END.

IF ttCriter.SortBy < 1 OR ttCriter.SortBy > 2 
THEN ttCriter.SortBy = 1. 

ASSIGN lcGrpHeader = (IF ttCriter.InvGroup = ""
                      THEN "ALL" 
                      ELSE ttCriter.InvGroup)
       jar         = valikko[ttCriter.SortBy]
       extcustgrp  = (CAN-FIND(FIRST tCustGroup)).


IF NOT extcustgrp THEN DO:
   ASSIGN
   cgcustno1 = 1
   cgcustno2 = 999999999.
END.

ELSE DO:
   ASSIGN cgcustno1 = 999999999
          cgcustno2 = 1. 

   FOR EACH TCustGroup,
       EACH cgmember NO-LOCK WHERE
            cgMember.Brand     = gcBrand AND
            cgmember.custgroup = Tcustgroup.custgroup:

      FIND FIRST tcgmember WHERE 
                 Tcgmember.custno = cgmember.custnum
      NO-LOCK NO-ERROR.

      IF NOT AVAIL tcgmember THEN DO:           
         CREATE Tcgmember.
         ASSIGN
         Tcgmember.custno = cgmember.custnum
         cgcustno1 = min(cgcustno1,cgmember.custnum)
         cgcustno2 = max(cgcustno2,cgmember.custnum).
      END.   

   END.
END. 

ASSIGN ryhma2 = string(ttCriter.Day11) + "-" + string(ttCriter.Day12)
       ryhma3 = string(ttCriter.Day21) + "-" + string(ttCriter.Day22)
       ryhma4 = string(ttCriter.Day31) + "-" + string(ttCriter.Day32)
       ryhma5 = string(ttCriter.Day41) + "-" + string(ttCriter.Day42)
       ryhma6 = string(ttCriter.Day51) + "-" + string(ttCriter.Day52)
       ryhma7 = "over " + string(ttCriter.DayOver).

IF ttCriter.ToFile = "" THEN ASSIGN        
       ryhma2 = fill(" ",8 - length(ryhma2)) + ryhma2
       ryhma3 = fill(" ",8 - length(ryhma3)) + ryhma3
       ryhma4 = fill(" ",8 - length(ryhma4)) + ryhma4
       ryhma5 = fill(" ",8 - length(ryhma5)) + ryhma5
       ryhma6 = fill(" ",8 - length(ryhma6)) + ryhma6
       ryhma7 = fill(" ",8 - length(ryhma7)) + ryhma7.

/* preselect inv.groups */
FOR EACH InvGroup NO-LOCK WHERE
         InvGroup.Brand = gcBrand AND
         (IF ttCriter.InvGroup = ""
          THEN TRUE
          ELSE InvGroup.InvGroup = ttCriter.InvGroup):

   CREATE ttInvGroup.
   ttInvGroup.InvGroup = InvGroup.InvGroup.

END.

/* headers to file */
IF ttCriter.ToFile > "" THEN DO:
   PUT STREAM tul UNFORMATTED
      "AGE ANALYSIS"                       SKIP
      "Balance on " ttCriter.RepDate  SKIP
      lcGrpHeader SKIP(1).

   IF NOT ttCriter.OnlySum 
   THEN PUT STREAM tul UNFORMATTED
      "Customer"        CHR(9)
      "Name"            CHR(9).

   PUT STREAM tul UNFORMATTED   
      "Debt total"      CHR(9)
      "Not overdue"     CHR(9)
      "Overdue " ryhma2 CHR(9)
      "Overdue " ryhma3 CHR(9)
      "Overdue " ryhma4 CHR(9)
      "Overdue " ryhma5 CHR(9)
      "Overdue " ryhma6 CHR(9)
      ryhma7            SKIP.
END.   

FOR EACH ttInvGroup,
    each Customer no-lock  where
         Customer.Brand    = gcBrand             AND
         Customer.InvGroup = ttInvGroup.InvGroup AND
         Customer.CustNum >= cgcustno1           AND
         Customer.CustNum <= cgcustno2           AND    
         (If NOT extcustgrp 
          THEN TRUE 
          ELSE CAN-FIND(FIRST tcgmember  where
                              tcgmember.custno = Customer.CustNum))
BY (if ttCriter.SortBy = 1 
    then string(CustNum,"999999999") 
    else Customer.CustName):

   assign
     asaldo = 0
     eiera  = 0
     era1   = 0
     era2   = 0
     era3   = 0
     era4   = 0
     era5   = 0
     era6   = 0
     liColl = liColl + 1.

   IF liColl MOD 100 = 0 AND NOT SESSION:BATCH THEN DO:
      PAUSE 0.   
      DISPLAY Customer.InvGroup LABEL "InvGroup"
              liColl LABEL "Count" FORMAT ">>>>>>>>9"
              WITH OVERLAY ROW 10 COL 50 TITLE " Collecting "
              FRAME fColl.
   END.

   for each Invoice NO-LOCK USE-INDEX CustNum WHERE
            Invoice.Brand    = gcBrand          AND
            Invoice.CustNum  = Customer.CustNum AND
            Invoice.InvDate <= ttCriter.RepDate and 
            /* printing not denied */
            lookup(string(Invoice.InvType),lcTypeDenied) = 0:

       ldInvBal = fCalcInvBal(BUFFER Invoice,
                              ttCriter.RepDate,
                              FALSE).

       if ldInvBal = 0 then next.
       if ldInvBal = ? then do:
          output stream slog to /tmp/age_err.log append.
          put stream slog unformatted 
             Invoice.InvNum chr(9)
             "Unknown value" skip.
          output stream slog close. 
          next.
       end.

       asaldo = asaldo + ldInvBal.
       if ttCriter.RepDate - Invoice.DueDate < 1 then assign
          eiera = eiera + ldInvBal.
       else if ttCriter.RepDate - Invoice.DueDate <= ttCriter.Day12 then assign
          era1  = era1  + ldInvBal.
       else if ttCriter.RepDate - Invoice.DueDate <= ttCriter.Day22 then assign
          era2  = era2  + ldInvBal.
       else if ttCriter.RepDate - Invoice.DueDate <= ttCriter.Day32 then assign
          era3  = era3  + ldInvBal.
       else if ttCriter.RepDate - Invoice.DueDate <= ttCriter.Day42 then assign
          era4  = era4  + ldInvBal.
       else if ttCriter.RepDate - Invoice.DueDate <= ttCriter.Day52 then assign
          era5  = era5  + ldInvBal.
       else if ttCriter.RepDate - Invoice.DueDate > ttCriter.DayOver then assign
          era6  = era6  + ldInvBal.
   end.

   if asaldo = 0 then next.

   assign
     yasaldo = yasaldo + asaldo
     yeiera  = yeiera  + eiera
     yera1   = yera1   + era1
     yera2   = yera2   + era2
     yera3   = yera3   + era3
     yera4   = yera4   + era4
     yera5   = yera5   + era5
     yera6   = yera6   + era6.

   /* specification / only summary */
   IF NOT ttCriter.OnlySum THEN DO:

      /* tarvitaanko uusi sivu */
      fChgPage(0).

      IF ttCriter.ToFile = "" THEN DO:
         display stream tul 
            Customer.CustNum Customer.CustName asaldo 
            eiera era1 era2 era3 era4 era5 era6 
         with frame avline.
         down stream tul with frame avline.

         /* line- ja katkolaskurit */
         assign
         rl = rl + 1
         rlx = rlx + 1.

         if rlx = 5 then do:
            /* tyhjA line joka 5:nnen vAliin */
            assign rl = rl + 1  rlx = 0.
            put stream tul skip(1).
         end.
      END.

      ELSE PUT STREAM tul UNFORMATTED
         Customer.CustNum    CHR(9)
         Customer.CustName   CHR(9)
         asaldo            CHR(9)
         eiera             CHR(9)
         era1              CHR(9)
         era2              CHR(9)
         era3              CHR(9)
         era4              CHR(9)
         era5              CHR(9)
         era6              SKIP.

   end.

end. /* print-line */

fChgPage(2).

/* summary */
IF ttCriter.ToFile = "" THEN DO:

   IF NOT ttCriter.OnlySum THEN DO:
      put stream tul viiva1 at 2 skip.
      rl = rl + 1.
   END.

   PUT STREAM tul "**     TOTAL"  at 2
                  yasaldo to 42 
                  yeiera  to 55 
                  yera1   to 68
                  yera2   to 81 
                  yera3   to 94 
                  yera4   to 107 
                  yera5   to 120 
                  yera6   to 133 
                  skip.

   rl = rl + 1.

   {Syst/uprfeed.i rl}
END.

/* summary line to file only if specification was not printed */
ELSE IF ttCriter.OnlySum THEN DO:

    PUT STREAM tul UNFORMATTED 
        yasaldo     CHR(9)
        yeiera      CHR(9)
        yera1       CHR(9)
        yera2       CHR(9)
        yera3       CHR(9)
        yera4       CHR(9)
        yera5       CHR(9)
        yera6       SKIP.

END.

IF ttCriter.ToFile > "" THEN DO:
   OUTPUT STREAM tul CLOSE.
   SESSION:NUMERIC-FORMAT = lcSessionNum.
END.   

HIDE FRAME fColl NO-PAUSE.

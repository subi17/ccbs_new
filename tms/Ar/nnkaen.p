/* --------------------------------------------------------------------------
  MODULE .......: NNKAEN.P
  FUNCTION .....: Kassaennusteen print-line
  SOVELLUTUS ...: TMS
  AUTHOR .......: TT
  CREATED ......: 11.07.1991
  changePVM ....: 08.03.94 /tt --> JAtetAAn as-limi[4] huomioimatta
                  17.05.94 /pt --> Logiikka muutettu toimivaksi
                  17.05.94 /pt --> Vaihtoehtoisesti valittavissa maksukAytOs
                  23.11.1997/pt InvGroup -rajaus
                  12.02.02. lp  changed swedish TO english 
                  02.09.02/aam  use invbal.p 
                  16.10.02/aam  CustNum format longer for sort
                  20.03.03/jp    External Customer Group
                  12.09.03/aam  brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}

{cparam2.i}
{utumaa.i "new"}

assign tuni1 = "nnkaen"
       tuni2 = "".

def var viiva1 as char format "x(114)".
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
def var jar as char format "x(30)".
DEF VAR order AS INT.
DEF VAR sl AS INT.
DEF VAR rl AS INT.
DEF VAR rlx AS INT.
DEF VAR lev AS INT init 114.
def var ke as log format "Yes/No" init FALSE.

def var asaldo as dec format "ZZZZZZ9.99-" init 0.
def var yasaldo as dec format "ZZZZZZZZZ9-" init 0.
def var amk  as dec format "ZZZZZ9.99-" EXTENT 5 init 0.
def var yamk  as dec format "ZZZZZZZZ9-" EXTENT 5 init 0.


DEF VAR InvGroup LIKE InvGroup.InvGroup NO-UNDO.
DEF VAR IGName LIKE InvGroup.IGName NO-UNDO.

def var ryhma2 as char format "x(7)".
def var ryhma3 as char format "x(7)".
def var ryhma4 as char format "x(7)".
def var ryhma5 as char format "x(8)".

def var raja as int format "ZZ9" EXTENT 4 NO-UNDO init[1,8,15,31].
DEF VAR et AS INT NO-UNDO.
DEF VAR ry AS INT NO-UNDO.

DEF VAR suoritettu LIKE asaldo.
DEF VAR salen LIKE asaldo.
DEF VAR velka LIKE asaldo.
def var pyynto as log format "Yes/No" init TRUE.
def var pytx as char format "x(20)".

def var valikko as char format "x(40)" EXTENT 2 init
["BY CUST. NUMBER","BY CUST. NAME"].
def var valik as char format "x(30)".
DEF VAR i AS INT.

def var lcTypeDenied as char  no-undo.

DEF VAR ExtCustGrp   AS LOG   NO-UNDO init FALSE FORMAT "Y/N".
DEF VAR extname      AS CHAR  NO-UNDO.
DEF VAR dExtCustGrp  AS CHAR  NO-UNDO FORMAT "x(25)".
DEF VAR kpl          AS INT   NO-UNDO.
DEF VAR CgCustno1    AS INT no-undo.
DEF VAR cgCustno2    AS INT no-undo.


DEF TEMP-TABLE TCustGroup
FIELD CustGroup LIKE CustGroup.CustGroup
INDEX CustGroup CustGroup.

DEF TEMP-TABLE TCGMember
FIELD custno LIKE customer.custnum
INDEX custno custno.



ASSIGN
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev)
lcTypeDenied = fCParamC("InvTypeDenied"). 

FIND FIRST Company no-lock no-error.

form
   valik NO-LABEL
   with overlay 2 down title color value(ctc) " CHOOSE ORDER FOR PRINTOUT "
   COLOR value(cfc) ROW 6 centered FRAME rival.

form
   skip(1)
   " Note: This program prints out a PAYMENT FORECAST according to "  SKIP
   "       given criteria."                                           skip(1)
   "       When required, the most likely date of payment for each invoice" 
                                                                       SKIP
   "       will be calculated according to customer's prior payment behaviour."    SKIP
   skip(11)
   WITH ROW 1 side-labels width 80
   title color value(ctc) " " + ynimi + " PAYMENT FORECAST " +
   string(pvm,"99-99-99") + " " COLOR value(cfc) FRAME valinta.

form header
   viiva1 AT 2 SKIP
   ynimi at 2 "PAYMENT FORECAST OF NOT PAID INVOICES" at 44 "Page" AT 108     
   sl format "ZZ9" SKIP
   "Inv.group" at 2 InvGroup IGName format "x(22)"
   jar + ", " + pytx at 44 format "x(60)"
   string(pvm,"99-99-99") AT 108 SKIP
   viiva2 AT 2 skip(1)

   "Cust."      AT 2         /* Customer.CustNum */
   "Cust."      AT 12        /* Customer.CustName */
   "Paym."      TO 38        /* Customer.PaymMethod */
   "Debt"       AT 46        /* asaldo */
   "Overdue"    AT 56        /* amk[1] */

   "Within"     AT 69        /* amk[2] */
   "Within"     AT 82        /* amk[3] */
   "Within"     AT 95        /* amk[4] */
   "Within"     AT 108 SKIP  /* amk[5] */


   "number"     AT 2
   "name"       AT 12
   "behav."     TO 38
   "total"      AT 45
   ryhma2       AT 69
   ryhma3       AT 82
   ryhma4       AT 95
   ryhma5       AT 108 SKIP

   viiva3 AT 2 SKIP
   WITH width 116 NO-LABEL no-box FRAME sivuots.

form
   Customer.CustNum   AT 2  format ">>>>>>>9"
   Customer.CustName  at 12 format "x(23)"
   Customer.PaymMethod    to 38 format "zz9+"
   asaldo           TO 50
   amk[1]           TO 63
   amk[2]           TO 76
   amk[3]           TO 89
   amk[4]           TO 102
   amk[5]           TO 115 SKIP
   WITH width 116 NO-LABELS no-box FRAME avline.

form

   InvGroup label "Invoice group ......."
   IGName format "x(24)" NO-LABEL                SKIP

   "External Cust groups.:" extcustgrp NO-LABEL
   HELP "Select external customer groups "
   extname format "x(30)" NO-LABEL               skip(1)
                 "1. Already due invoices "      SKIP
   raja[1] label "2. Within            "  "days" SKIP
   raja[2] label "3. Within            "  "days" SKIP
   raja[3] label "4. Within            "  "days" SKIP
   raja[4] label "5. Within            "  "days" SKIP
                 "6. Later "                     SKIP
   pyynto  label "Notice payment behaviour" SKIP

   with title color value(ctc) " CRITERIA FOR PRINTOUT "  side-labels
   COLOR value(cfc) ROW 8 centered OVERLAY FRAME rajat.

cfc = "sel". RUN ufcolor.
view FRAME valinta.
cfc = "puli". RUN ufcolor.
PAUSE 0 no-message.

display raja pyynto "ALL" @ IGName WITH FRAME rajat.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      ASSIGN ufk = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.

      IF toimi = 1 THEN DO:
         ehto = 9. RUN ufkey.
         UPDATE InvGroup extcustgrp
            raja[1]
            raja[2]
            validate(input raja[2] > input raja[1], "Impossible definition !")
            raja[3]
            validate(input raja[3] > input raja[2], "Impossible definition !")
            raja[4]
            validate(input raja[4] > input raja[3], "Impossible definition !")
            pyynto
         WITH FRAME rajat EDITING:
            READKEY. nap = keylabel(LASTKEY).
            IF lookup(nap,poisnap) > 0 THEN DO:
               HIDE MESSAGE.
               if frame-field = "InvGroup" THEN DO:
                  ASSIGN FRAME rajat InvGroup.
                  if InvGroup = "" THEN DO:
                     disp "ALL" @ IGName WITH FRAME rajat.
                     IGName = "ALL".
                  END.
                  ELSE DO:
                     FIND InvGroup where
                          InvGroup.Brand    = gcBrand AND
                          InvGroup.InvGroup = InvGroup
                     no-lock no-error.
                     IF NOT AVAIL InvGroup THEN DO:
                        bell.  message "Unknown Invoicing Group !".
                        NEXT.
                     END.
                     DISP InvGroup.IGName @ IGName WITH FRAME rajat.
                     IGName = InvGroup.IGName.
                  END.
               END.
               ELSE if frame-field = "extcustgrp" then do:
                  ASSIGN
                     cgcustno1 = 999999999
                     cgcustno2 = 0.

                  FOR EACH TCustGroup.
                      DELETE TCustGroup.
                  END.

                  FOR EACH TCGMember.
                      DELETE TCGMEMBER.
                  END.
                                                                                                  assign frame rajat extcustgrp.

                  if extcustgrp NE YES   then do:
                     disp "NOT SELECTED" @ extname with frame rajat.
                  end.
                  else do:
                     RUN gathecg(INPUT-OUTPUT table TCustGroup).
                     /* DISPLAY Customer groups */
                     EHTO = 9.
                     run ufkey.
                     FOR EACH TCustGroup.
                        dExtCustGrp = dExtCustGrp + TCustGroup.CustGroup +
                        ",".
                     END.
                     /* Remove last comma */

                     IF dextcustgrp ne "" THEN
                        dextcustgrp = SUBSTRING(DextCustGrp,1,                                                            LENGTH(dextCustGrp) - 1).
                     disp ExtCustGrp dExtCustGrp @ extname with frame rajat.

                     IF  dExtCustGrp = "" THEN
                     disp "NOT SELECTED" @ extname with frame rajat.

                     apply 13 /* ENTER*/ .
                  end.
               END.
            END.
            APPLY LASTKEY.
         END.

         NEXT toimi.
      END.

      IF toimi = 5 THEN DO:
         if pyynto then pytx = "PAYMENT BEHAVIOUR NOTICED".
         else           pytx = "PAYMENT BEHAVIOUR NOT NOTICED".
         cfc = "uusi". RUN ufcolor.   ccc = cfc.
         DO i = 1 TO 2 WITH FRAME rival:
            valik = valikko[i].
            DISPLAY valik.
            IF i < 2 THEN DOWN.
            ELSE up 1.
         END.

rival:      repeat ON ENDKEY UNDO rival, LEAVE rival WITH FRAME rival:
               MESSAGE
               "Choose printing order, press ENTER !".
               READKEY PAUSE 0.
               CHOOSE ROW valik ;(uchoose.i;) no-error.
               COLOR DISPLAY value(ccc) valik WITH FRAME rival.
               i = FRAME-LINE.
               HIDE MESSAGE no-pause.
               ASSIGN order = i
                      jar  = valikko[i].
               if lookup(keylabel(lastkey),"enter,return") > 0 THEN LEAVE rival.
            END.  /* rival */
            CLEAR FRAME rival ALL no-pause.
            HIDE FRAME rival no-pause.
            LEAVE toimi.
      END.
      IF toimi = 8 THEN RETURN.
   END. /* toimi */

ASSIGN tila = TRUE.
{tmsreport.i "return"}

ASSIGN
sl = 0
rl = skayt1.

ASSIGN
  ryhma2  = string(raja[1]) + "-" + string(raja[2] - 1)
  ryhma3  = string(raja[2]) + "-" + string(raja[3] - 1)
  ryhma4  = string(raja[3]) + "-" + string(raja[4] - 1)
  ryhma5  = "over " + string(raja[4] - 1,"zz9").

FOR EACH TCustGroup.
   FOR EACH cgmember WHERE
            cgMember.Brand     = gcBrand AND
            cgmember.custgroup = Tcustgroup.custgroup
   NO-lock.
      FIND FIRST tcgmember WHERE
                 Tcgmember.custno = cgmember.custnum
      NO-LOCK NO-ERROR.
      IF NOT AVAIL tcgmember THEN DO:
         CREATE Tcgmember.
         ASSIGN
            Tcgmember.custno = cgmember.custnum
            kpl = kpl  + 1
            cgcustno1 = min(cgcustno1,cgmember.custnum)
            cgcustno2 = max(cgcustno2,cgmember.custnum).
      END.
   ENd.
END. 

if extcustgrp = FALSE THEN DO:
   ASSIGN
   cgcustno1 = 1
   cgcustno2 = 999999999.
END.

message "Printing in process - cancel, press 'END'".
runko : repeat:
for each Customer no-lock where 
         Customer.Brand    = gcBrand          AND
         Customer.CustNum >= cgcustno1        AND
         Customer.CustNum <= cgcustno2        AND
         (if InvGroup ne "" THEN
         Customer.InvGroup = InvGroup ELSE TRUE) AND
         (If NOT extcustgrp THEN TRUE
          ELSE CAN-FIND(FIRST tcgmember  where
          tcgmember.custno = Customer.CustNum))         
by 
(if order = 1 then string(Customer.CustNum,"99999999") ELSE customer.CustName):

   ASSIGN asaldo = 0 amk = 0.

   IF NOT can-find(FIRST Invoice of Customer) THEN NEXT.

   FOR EACH Invoice of Customer NO-LOCK WHERE
      /* printing not denied */
      lookup(string(Invoice.InvType),lcTypeDenied) = 0:

       /* yhden myyntilaskun kAsittely */

       RUN invbal (Invoice.InvNum, OUTPUT velka). 

       IF velka = 0 THEN NEXT.

       /* montako pAivAA tAstA pAivAstA on maksupAivAAn ? */
       et = Invoice.DueDate - pvm + IF pyynto THEN Customer.PaymMethod ELSE 0.
       /* mihin ryhmAAn summa laitetaan ? */
       ry = 1.
       DO i = 4 TO 1 BY -1.
          IF et >= raja[i] THEN DO:
             ry = i + 1.
             LEAVE.
          END.
       END.
       /* nyt ry osoittaa amk-tauluun */
       ASSIGN
       asaldo   = asaldo   + velka
       yasaldo  = yasaldo  + velka
       amk [ry] = amk [ry] + velka
       yamk[ry] = yamk[ry] + velka.

   END.

   /* onko kjA pyytAnyt keskeytystA ? */
   READKEY PAUSE 0.
   nap = keylabel(LASTKEY).
   if nap = "END" THEN DO:
      message "Are You sure You want to cancel printing ? (Y/N) "
      UPDATE ke.
      IF ke THEN DO:
         display stream tul "Printing cancelled !" WITH NO-LABEL no-box.
         rl = rl + 1.
         LEAVE runko.
      END.
   END.

   /* tarvitaanko uusi sivu */
   IF rl >= skayt1 THEN DO:
      IF sl > 0 THEN PUT STREAM tul skip(spit1 - rl).
      ASSIGN rlx = 0 sl = sl + 1 rl = 8. view STREAM tul FRAME sivuots.
   END.

   IF asaldo = 0 THEN NEXT.
   DISPLAY STREAM tul Customer.CustNum Customer.CustName Customer.PaymMethod
                      asaldo amk[1] amk[2] amk[3] amk[4] amk[5]
                      WITH FRAME avline.
   DOWN STREAM tul WITH FRAME avline.

   /* line- ja katkolaskurit */
   ASSIGN rl = rl + 1 rlx = rlx + 1.
   IF rlx = 5 THEN DO:
      /* tyhjA line joka 5:nnen vAliin */
      ASSIGN rl = rl + 1  rlx = 0.
      PUT STREAM tul skip(1).
   END.

END. /* print-line */

/* Loppusummien print-line */

PUT STREAM tul viiva1 AT 2 SKIP
               "**     TOTAL         " AT 2
               yasaldo TO 50 yamk[1]  TO 63 yamk[2] TO 76
               yamk[3]   TO 89 yamk[4]   TO 102 yamk[5] TO 115 SKIP.

ASSIGN rl = rl + 2.

LEAVE runko.
END. /* runko */
/* vielA viimeinen sivu kohdalleen */
PUT STREAM tul skip(spit1 - rl).

ASSIGN tila = FALSE.
{tmsreport.i}

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.


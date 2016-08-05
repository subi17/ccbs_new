/*-----------------------------------------------------------------------------
  MODULE .......: NNIKAJ.P
  FUNCTION .....: Myyntisaamisten ikAjakauman print-line
  SOVELLUTUS ...: VP
  AUTHOR .......: TT
  CREATED ......: 09.07.1991
  changePVM ....: 14.08.1991/tt
                  23.11.1997/pt  InvGroup -rajaus
                  08.07.1998/kl  english
                  04.06.1999/kl  day-to
                  09.11.2000/ht  eiera- and asaldo-values to 10 millions
                  11.02.02 lp    changed swedish to english
                  08.03.02/aam   longer format for as-nro in by-sort 
                  17.06.2002/aam skip invoices defined in parameter 
                                 InvTypeDenied
                  02.09.2002/aam use invbal.p 
                  13.01.2002/jp  External customer group
                  20.05.2003/aam use finvbal
                  20.08.2003/aam 2 additional day columns,
                                 optionally to Excel,
                                 optionally only summary,
                                 logic to ageanal.p
                  07.06.2004/aam brand to InvGroup               
                  07.03.2005/aam "invoices before" -> "balance on"
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i "new"}
{Ar/ageanal.i}

assign tuni1 = "nnikaj"
       tuni2 = "".

def var ke as log format "Yes/No" init false no-undo.

def var InvGroup like invgroup.InvGroup no-undo.
def var IGName like invgroup.IGName no-undo.
def var day-to  as   da               no-undo init today.

def var aryhma2 as int format ">>9" no-undo.
def var aryhma3 as int format ">>9" no-undo.
def var aryhma4 as int format ">>9" no-undo.
def var aryhma5 as int format ">>9" no-undo.
def var aryhma6 as int format ">>9" no-undo.
def var aryhma7 as int format ">>9" no-undo.

def var lryhma2 as int format ">>9" no-undo.
def var lryhma3 as int format ">>9" no-undo.
def var lryhma4 as int format ">>9" no-undo.
def var lryhma5 as int format ">>9" no-undo.
def var lryhma6 as int format ">>9" no-undo.

DEF VAR dExtCustGrp   AS CHAR  NO-UNDO FORMAT "x(25)".
DEF VAR kpl           AS INT   NO-UNDO.
DEF VAR ExtCustGrp    AS LOG   NO-UNDO init FALSE FORMAT "X/-".
DEF VAR extname       AS CHAR  NO-UNDO.
DEF VAR lcGrpHeader   AS CHAR  NO-UNDO. 
DEF VAR lcFile        AS CHAR  NO-UNDO. 
DEF VAR llOnlySummary AS LOG   NO-UNDO. 

DEF VAR i             AS INT   NO-UNDO.
DEF VAR valik         AS CHAR  NO-UNDO FORMAT "X(30)".
def var order         as int   no-undo.


form
   valik no-label
   with overlay 2 down title color value(ctc) " CHOOSE ORDER FOR PRINTOUT "
   color value(cfc) row 6 centered frame rival.

form
   "  Note : This program creates an AGE ANALYSIS of all "    skip
   "         unpaid invoices according to given criteria:"    skip
   skip(15)
   with row 1 side-labels width 80
        title color value(ctc) " " + ynimi + " AGE ANALYSIS " +
        string(pvm,"99-99-99") + " " color value(cfc)
        frame valinta.

form
   "Balance on ....:" day-to  no-label 
      help "Balance on given date will be listed"
      format "99-99-99"  skip
   "Inv. group.....:" InvGroup no-label help "Customers in invoicing group"
                      IGName no-label format "x(30)"     skip
   "ExtCust.group..:" extcustgrp no-label  
      HELP "X = Select external customer groups "
      Extname no-label format "x(30)"                     skip
   "Only summary ..:" 
      llOnlySummary NO-LABEL 
         FORMAT "Yes/No"
         HELP "Print only summary page"
      SKIP
   "Excel-file ....:"
      lcFile NO-LABEL
         FORMAT "X(40)"
         HELP "If name is given, then a tab-separated file is made"
      SKIP(1)

   "Group 1 : Not overdue invoices" skip
   aryhma2 label "Group 2 " "days" "  - " lryhma2 no-label "days" skip
   aryhma3 label "Group 3 " "days" "  - " lryhma3 no-label "days" skip
   aryhma4 label "Group 4 " "days" "  - " lryhma4 no-label "days" skip
   aryhma5 label "Group 5 " "days" "  - " lryhma5 no-label "days" skip
   aryhma6 label "Group 6 " "days" "  - " lryhma6 no-label "days" skip
   "Group 7 :" aryhma7 no-label "days overdue invoices" 

   with title color value(ctc) " CRITERIA FOR PRINTOUT " side-labels
   color value(cfc) row 4 centered overlay frame rajat.

cfc = "sel". RUN Syst/ufcolor.
view frame valinta.
cfc = "puli". RUN Syst/ufcolor.
pause 0 no-message.

assign
  aryhma2 = 1  lryhma2 = 7 
  aryhma3 = 8  lryhma3 = 30
  aryhma4 = 31 lryhma4 = 60 
  aryhma5 = 61 lryhma5 = 90
  aryhma6 = 91 lryhma6 = 180
  aryhma7 = lryhma6.

display
   aryhma2 lryhma2 
   aryhma3 lryhma3 
   aryhma4 lryhma4 
   aryhma5 lryhma5
   aryhma6 lryhma6
   aryhma7 
   llOnlySummary
   day-to
   "ALL" @ IGName
   "NOT SELECTED" @ extname 
with frame rajat.

toimi:
repeat with frame valinta on endkey undo toimi, next toimi:
      assign ufk = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.

      if toimi = 1 then do:
         ehto = 9. RUN Syst/ufkey.
         update 
            day-to
            InvGroup
            extcustgrp
            llOnlySummary
            lcFile
            lryhma2
            validate(input lryhma2 >  input aryhma2, "Impossible definition !")
            lryhma3
            validate(input lryhma3 >  input aryhma3, "Impossible definition !")
            lryhma4
            validate(input lryhma4 >  input aryhma4, "Impossible definition !")
            lryhma5
            validate(input lryhma5 >  input aryhma5, "Impossible definition !")
            lryhma6
            validate(input lryhma6 >  input aryhma6, "Impossible definition !")

         with frame rajat editing:
            readkey. nap = keylabel(lastkey).
            if lookup(nap,poisnap) > 0 then do:
               hide message.
               if frame-field = "InvGroup" then do:
                  assign frame rajat InvGroup.
                  if InvGroup = "" then do:
                     disp "ALL" @ IGName with frame rajat.
                     IGName = "ALL".
                  end.
                  else do:
                     find invgroup where 
                          InvGroup.Brand    = gcBrand AND
                          invgroup.InvGroup = InvGroup
                     no-lock no-error.
                     if not avail invgroup then do:
                        bell.  message "Unknown Invoicing Group !".
                        next.
                     end.

                     disp invgroup.IGName @ IGName with frame rajat.
                     IGName = invgroup.IGName.
                  end.
               end.

               ELSE if frame-field = "extcustgrp" then do:

                  FOR EACH TCustGroup.
                     DELETE TCustGroup.
                  END.

                  assign frame rajat extcustgrp.

                  if extcustgrp = FALSE  then do:
                     disp "NOT SELECTED" @ extname with frame rajat.
                  end.
                  else do:
                     RUN Mc/gathecg(INPUT-OUTPUT table TCustGroup).
                     /* DISPLAY Customer groups */
                     EHTO = 9.
                     RUN Syst/ufkey.
                     FOR EACH TCustGroup.
                        dExtCustGrp = dExtCustGrp + TCustGroup.CustGroup + ",".
                     END.
                     /* Remove last comma */

                     IF dextcustgrp ne "" THEN
                        dextcustgrp = SUBSTRING(DextCustGrp,1,
                                         LENGTH(dextCustGrp) - 1). 
                     disp ExtCustGrp dExtCustGrp @ extname with frame rajat.

                     IF  dExtCustGrp = "" THEN 
                     disp "NOT SELECTED" @ extname with frame rajat. 

                     apply 13 /* ENTER*/ .
                  end.
               end.  

               ELSE IF FRAME-FIELD = "lryhma2" THEN DO:
                  aryhma3 = INPUT FRAME rajat lryhma2 + 1.
                  DISPLAY aryhma3 WITH FRAME rajat.
               END.
               ELSE IF FRAME-FIELD = "lryhma3" THEN DO:
                  aryhma4 = INPUT FRAME rajat lryhma3 + 1.
                  DISPLAY aryhma4 WITH FRAME rajat.
               END.
               ELSE IF FRAME-FIELD = "lryhma4" THEN DO:
                  aryhma5 = INPUT FRAME rajat lryhma4 + 1.
                  DISPLAY aryhma5 WITH FRAME rajat.
               END.
               ELSE IF FRAME-FIELD = "lryhma5" THEN DO:
                  aryhma6 = INPUT FRAME rajat lryhma5 + 1.
                  DISPLAY aryhma6 WITH FRAME rajat.
               END.
               ELSE IF FRAME-FIELD = "lryhma6" THEN DO:
                  aryhma7 = INPUT FRAME rajat lryhma6.
                  DISPLAY aryhma7 WITH FRAME rajat.
               END.

            END.
            apply lastkey.
         end.

      end.

      else if toimi = 5 then do:
         IF NOT llOnlySummary THEN DO:
            assign
            cfc = "uusi". RUN Syst/ufcolor.   ccc = cfc.
            do i = 1 to 2 with frame rival:
               valik = valikko[i].
               display valik.
               if i < 2 then down.
               else up 1.
            end.

            rival:      
            repeat on endkey undo rival, leave rival with frame rival:
               message
               "Choose printing order, press ENTER !".
               readkey pause 0.
               choose row valik {Syst/uchoose.i} no-error.
               color display value(ccc) valik with frame rival.
               i = frame-line.
               hide message no-pause.
               assign order = i.
               if lookup(keylabel(lastkey),"enter,return") > 0 then leave rival.
            end.  /* rival */

            clear frame rival all no-pause.
            hide frame rival no-pause.
         end.

         ELSE order = 1.

         leave toimi.
      end.

      else if toimi = 8 then return.

end. /* toimi */

ehto = 5.
RUN Syst/ufkey.

CREATE ttCriter.
ASSIGN ttCriter.InvGroup = InvGroup
       ttCriter.RepDate  = day-to
       ttCriter.Day11    = aryhma2
       ttCriter.Day12    = lryhma2
       ttCriter.Day21    = aryhma3
       ttCriter.Day22    = lryhma3
       ttCriter.Day31    = aryhma4
       ttCriter.Day32    = lryhma4
       ttCriter.Day41    = aryhma5
       ttCriter.Day42    = lryhma5
       ttCriter.Day51    = aryhma6
       ttCriter.Day52    = lryhma6
       ttCriter.DayOver  = aryhma7
       ttCriter.ToFile   = lcFile
       ttCriter.OnlySum  = llOnlySummary
       ttCriter.SortBy   = order.

IF extcustgrp = FALSE THEN 
EMPTY TEMP-TABLE TCustGroup.

IF ttCriter.ToFile = "" THEN DO:
   assign tila = true.
   {Syst/utuloste.i "return"}
END.

message "Printing in process".

RUN Ar/ageanal.p (INPUT TABLE TCustGroup,
             INPUT TABLE ttCriter).

IF ttCriter.ToFile = "" THEN DO:
   assign tila = false.
   {Syst/utuloste.i}
END.   

MESSAGE "Age analysis report has been printed."
VIEW-AS ALERT-BOX
TITLE " Finished ".

hide message no-pause.
hide frame valinta no-pause.

/*------------------------------------------------------
  MODULE .......: NNPULA.P
  FUNCTION .....: Puhelut yhteens{- laskelma kuvaruudulle
  SOVELLUTUS ...: TN / NN
  AUTHOR .......: PT
  CREATED ......: 02.04.1996 pt
  MODIFIED .....: 23.02.1997 pt korj. virhe : minuutit oli sekunteja
  MODIFIED .....: 15.04.1997 pt nAytetAAn 3 eri ryhmAA kaikki/laskut./ei-lask.
                  15.12.1998 pt in English, inv. group
                  17.03.1999 kl separate Calls form NOT Billed customers
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF VAR i AS INT.
def var date1 as Date format "99-99-9999" NO-UNDO.
def var date2 as Date format "99-99-9999" NO-UNDO.

def var kpl  as int  no-undo format "zzz,zzz,zz9"  EXTENT 4.
def var minu as int  no-undo format "zzz,zzz,zz9"  EXTENT 4.
def var bmk  as dec  no-undo format "zzz,zzz,zz9"  EXTENT 4.
def var nmk  as dec  no-undo format "zzz,zzz,zz9"  EXTENT 4.
def var alep as dec  no-undo format "zzzz,zz9.99"  EXTENT 4.
DEF VAR InvGroup LIKE InvGroup.InvGroup NO-UNDO.
DEF VAR invd    LIKE InvGroup.BillPerm NO-UNDO.

def var ok as log no-undo format "Yes/No".

form
 skip(1)
 "         This program produces a quick summary of telephone Calls "  SKIP
 "         made between 2 dates given below."                          skip(2)
 "         Calls during .................:"   date1 no-label format "99-99-99"
 help "Earliest call date" "-"                date2 no-label format "99-99-99"
 help "Latest call date" SKIP
 SKIP    
 "         Customers in Invoicing Group..:" InvGroup
 help "Code of an invoicing group;  EMPTY: All customers"
 InvGroup.IGName no-label format "x(24)" skip(2)

 "  All Calls      Billed  Not Billed  No invoice" AT 27 SKIP
 "----------- ----------- ----------- -----------" AT 27 SKIP
   "Number of Calls:" AT 10 kpl [1]  kpl[2]  kpl[3]  kpl[4] SKIP
   "Total minutes .:" AT 10 minu[1] minu[2] minu[3] minu[4] SKIP
   "Amount ex Disc.:" AT 10 bmk [1]  bmk[2]  bmk[3]  bmk[4] SKIP
   "Net Qty Billed.:" AT 10 nmk [1]  nmk[2]  nmk[3]  nmk[4] SKIP
   "Average Disc. %:" AT 10 alep[1] alep[2] alep[3] alep[4] skip(1)

with row 1 width 80 title " QUICK SUMMARY OF Calls " NO-LABELS
   FRAME summary.

/* default dates FOR query */
date2 = date(month(TODAY),1,year(TODAY)) - 1.
date1 = date(month(date2),1,year(date2)).

DO WITH FRAME summary:
   PAUSE 0 no-message.
   disp "ALL" @ InvGroup.IGName.

   ehto = 9. RUN ufkey.
   UPDATE 
   date1 
   date2 validate(input date2 >= input date1,"Invalid order !")
   InvGroup validate(input InvGroup = "" OR 
                    can-find(InvGroup where InvGroup.InvGroup = INPUT InvGroup),
                    "Invalid Group !").
   if InvGroup ne "" THEN DO:
      FIND InvGroup where InvGroup.InvGroup = InvGroup no-lock.
      DISP InvGroup.IGName.
   END.   
END.

ok = FALSE.
message "Are You SURE that You want to start (Y/N) ? " UPDATE ok.

IF ok THEN DO:
   ufk = 0. ehto = 4. RUN ufkey.
   message "Calculating ...".    
   FOR EACH FixCDR no-lock where 
            FixCDR.Date >= date1 AND 
            FixCDR.Date <= date2,

      FIRST InvSeq no-lock where
            InvSeq.InvSeq = FixCDR.InvSeq.

      FIND Customer where 
           Customer.CustNum = FixCDR.InvCust 
      no-lock no-error.
      if InvGroup ne "" AND Customer.InvGroup NE InvGroup THEN NEXT.

      FIND FIRST InvGroup where
                 InvGroup.InvGroup = Customer.InvGroup
      no-lock no-error.
      IF AVAIL InvGroup THEN invd = InvGroup.BillPerm.
      ELSE invd = FALSE.

      PUT SCREEN ROW 23 col 40 string(i).
      ASSIGN i = i + 1.

      /* ALL Calls */
      ASSIGN
         kpl [1] = kpl [1] + 1
         minu[1] = minu[1] + Duration    /* divide later BY 60 */
         bmk [1] = bmk [1] + GrossPrice
         nmk [1] = nmk [1] + GrossPrice - DiscValue.

      IF invd THEN DO:
         IF InvSeq.Billed THEN
            /* Billed Calls */
            ASSIGN
               kpl [2] = kpl [2] + 1
               minu[2] = minu[2] + Duration    /* divide later BY 60 */
               bmk [2] = bmk [2] + GrossPrice
               nmk [2] = nmk [2] + GrossPrice - DiscValue.

         /* Unbilled Calls */
         ELSE
            ASSIGN
               kpl [3] = kpl [3] + 1
               minu[3] = minu[3] + Duration /* divide later BY 60 */
               bmk [3] = bmk [3] + GrossPrice
               nmk [3] = nmk [3] + GrossPrice - DiscValue.
      END.
      ELSE
         /* Unbilled Calls */
         ASSIGN
            kpl [4] = kpl [4] + 1
            minu[4] = minu[4] + Duration    /* divide later BY 60 */
            bmk [4] = bmk [4] + GrossPrice
            nmk [4] = nmk [4] + GrossPrice - DiscValue.
   END.

   DO i = 1 TO 4.
      alep[i] = 0.
      IF bmk[i] NE 0 THEN alep[i] = (bmk[i] - nmk[i]) / bmk[i] * 100.
      minu[i] = minu[i] / 60.
   END.

   DISPLAY
     kpl
     minu
     bmk
     nmk
     alep
   WITH FRAME summary.

   HIDE MESSAGE no-pause.
   message "Press ENTER !".
   PAUSE no-message.
END.
HIDE FRAME summary no-pause.


/* -------------------------------------------------------------------------
  MODULE .......: MSISDNi2.p
  FUNCTION .....: Mobile Subscriber ISDN number invoice VALUE                 
  APPLICATION ..: NN & TN
  CREATED ......: 03.07.2007 vk based on the msisdniv.p-module, but this
                                is for prepaid calls
  MODIFIED      : 04.10.2007 vk prepcdr.Charge field will be used without                                        deviding the sum by 10000
  Version ......: M15
  ------------------------------------------------------------------------ */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Mobcdr'}
{Func/callquery.i}

DEF INPUT PARAMETER    MsSeq LIKE MobSub.MsSeq  NO-UNDO.

DEF TEMP-TABLE ttCall NO-UNDO LIKE Prepcdr
   FIELD CDRTable AS CHAR.
   
DEF VAR alvpros   AS DECI NO-UNDO init 22.
DEF VAR lkm1      AS INT  NO-UNDO.
DEF VAR lkm2      AS INT  NO-UNDO.
def var summa1    AS DECI NO-UNDO FORMAT "-z,zzz,zzz.99".
def var summa2    AS DECI NO-UNDO FORMAT "-z,zzz,zzz.99".
def var summa1a   AS DECI NO-UNDO FORMAT "-z,zzz,zzz.99".
def var summa2a   AS DECI NO-UNDO FORMAT "-z,zzz,zzz.99".
def var pvm1      AS DATE NO-UNDO FORMAT "99.99.99".
def var pvm2      AS DATE NO-UNDO FORMAT "99.99.99".
DEF VAR lccli     AS CHAR NO-UNDO.
DEF VAR ldCurBal  AS DECI NO-UNDO FORMAT "-zzz,zzz.99".
DEF VAR liRound   AS INT  NO-UNDO.
DEF VAR liRight   AS INT  NO-UNDO.
DEF VAR lcBalance AS CHAR NO-UNDO.
DEF VAR lhCDR     AS HANDLE NO-UNDO.
DEF VAR liError   AS INT  NO-UNDO.

lhCDR = TEMP-TABLE ttCall:HANDLE. 

FIND MobSub where MobSub.MsSeq = MsSeq no-lock no-error.

if avail mobsub then lccli = mobsub.cli.
ELSE  DO:
   find first msowner where 
              msowner.msseq = msseq NO-LOCK NO-ERROR.
   IF avail msowner then lccli = msowner.cli.
END.   


form
   skip(1)
"  Note: This program shows the total value of mobile calls  " SKIP
"        made by this Mobile Subscriber.                     " SKIP(1)
"  Calls within .......:" pvm1
help "Earliest Date of call" "-" pvm2 
help "Latest Date of call" skip(1)

"  Prepaid " lkm1 "calls, " summa1 "euros"         SKIP(1)
"  Current balance              " ldCurBal "euros" SKIP(1)  
WITH
   row 7 col 9 overlay no-labels title " " + ynimi +
   "  Total Value of Calls,  "  + string(substr(lcCLI,1,16)) + ") " FRAME rajat.


pvm1 = date(month(TODAY),1,year(TODAY)).
pvm2 = pvm1 + 40.
pvm2 = date(month(pvm2),1,year(pvm2)) - 1.


rajat:
repeat WITH FRAME rajat:

   PAUSE 0.
   ehto = 9. RUN Syst/ufkey.p.

   UPDATE pvm1 pvm2
   validate (input pvm2 >= input pvm1,"Incorrect order !").

   toimi:
   repeat WITH FRAME toimi:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN LEAVE toimi.
   END.
   ASSIGN lkm1 = 0 lkm2 = 0 summa1 = 0 summa2 = 0.

   MESSAGE "Calculating ...".                   

   EMPTY TEMP-TABLE ttCall.
    
   fMobCDRCollect(INPUT "pre",
                  INPUT gcBrand,
                  INPUT katun,
                  INPUT pvm1,
                  INPUT pvm2,
                  INPUT 0,
                  INPUT "",
                  INPUT lcCLI,
                  INPUT 0,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT "",
                  INPUT 0,
                  INPUT-OUTPUT liError,
                  INPUT-OUTPUT lhCDR).
                              
   FOR EACH ttCall:
         IF ttCall.invseq    = 0 OR 
            ttCall.Errorcode > 0 THEN NEXT.
         
         ASSIGN lkm1   = lkm1 + 1
              summa1 = summa1 + ttCall.Charge.
   END.
   
   FIND LAST ttCall USE-INDEX CLI NO-ERROR.

   IF AVAIL ttCall THEN DO:

       RUN Mm/cdr_detail_value.p("PrepCDR",
                              ttCall.DateSt,
                              ttCall.DtlSeq,
                              "Balance after",
                              OUTPUT lcBalance).

       ldCurBal = DECIMAL(lcBalance) / 10000.
   END.                             
      
   DISPLAY 
     lkm1
     summa1
     ldCurBal.

   /* Calculating..-text away */ 
   HIDE MESSAGE NO-PAUSE.

   MESSAGE "Press ENTER to continue !".
   PAUSE NO-MESSAGE.
   LEAVE rajat.
END.

HIDE MESSAGE NO-PAUSE.
HIDE FRAME rajat NO-PAUSE.

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(lhCDR) THEN DELETE OBJECT lhCDR NO-ERROR.



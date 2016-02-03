{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/date.i}
{Inv/combine_invseq.i}

DEF VAR idafromdate as date no-undo.
DEF VAR idatodate as date no-undo.

ASSIGN
   idafromdate = date(month(today), 1, YEAR(TODAY))
   idatodate = fLastDayOfMonth(today).

DEFINE VARIABLE lcInfo AS CHARACTER NO-UNDO. 

lcInfo = "COMBINE INVSEQ".
form
   idaFromDate label  "From..."
      help "Invoice period from date" skip
   idatodate label  "To....."
      help "Invoice period to date"
WITH  OVERLAY ROW 8 centered
TITLE  lcInfo side-LABELS
FRAME a.

disp
   idatodate
   idafromdate with frame a.
   
DEFINE VARIABLE ok AS LOGICAL NO-UNDO. 
ok = false.

LOOP:
DO WHILE TRUE:

   ehto = 9. RUN ufkey.
   REPEAT ON ENDKEY UNDO, LEAVE:
      UPDATE 
         idafromdate
         idatodate with frame a.

      MESSAGE 
      "Start combining invoice sequences" skip
      idaFromDate "-" idaToDate "?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      UPDATE ok.

      if ok then leave LOOP.
   END.
   
   RETURN.
END.

def var licust as int no-undo.
def var licdr as int no-undo.
def var lidone as int no-undo.

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.
def var l as int no-undo.

pause 0.
disp i j k l with 1 down row 1 frame fcount.
pause 0.
disp licdr with 1 down row 10 frame fcdr.

for each mobsub no-lock where
         brand = "1" and
         paytype = false:
         
   run pfindinvseq(mobsub.invcust,mobsub.msseq).
end.
for each termmobsub no-lock where
         brand = "1" and
         paytype = false:
         
   run pfindinvseq(termmobsub.invcust,termmobsub.msseq).
end.

pause 0.
disp i j k l with frame fcount. 
pause.

hide frame fcount.
hide frame fcdr.
hide frame a.
pause 0.

procedure pfindinvseq:

   def input parameter iiinvcust as int no-undo.
   def input parameter iimsseq as int no-undo.
   
   i = i + 1.
         
   k = 0 .
   for each invseq no-lock where
            invseq.msseq = iimsseq and
            invseq.custnum = iiinvcust and
            invseq.billed = false and
            invseq.todate = idatodate:
      k = k + 1.
      j = j + 1.
   end.

   if k > 1 then do:
      run pcombineinvseq(iiinvcust,iimsseq,idafromdate,idatodate,
                         output lidone).
      licdr = licdr + lidone.
      pause 0.
      disp licdr with frame fcdr.
      l = l + 1.
   end.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j k l with 1 down frame fcount.
   end.
                
end.   


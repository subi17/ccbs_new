{Syst/testpaa.i}
katun = "Qvantel".

{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhfixedfee AS HANDLE NO-UNDO.
   lhfixedfee = BUFFER fixedfee:HANDLE.
   RUN StarEventInitialize(lhfixedfee).
END.

def stream sread.
input stream sread from /apps/yoigo/tms_support/201011/aam_yob316.log.

def var lcline as char no-undo.
def var limsseq as int no-undo.
def var lcevent as char no-undo.

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.


repeat:

   import stream sread unformatted lcline.
   
   assign
      limsseq = int(entry(2,lcline,chr(9)))
      lcevent = entry(4,lcline,chr(9)) no-error.
   if error-status:error or limsseq = 0 then next.
   
   i = i + 1.
   
   if lookup("mdubact",lcevent) > 0 then do:
    
      j = j + 1.
      for each fixedfee exclusive-lock use-index hosttable where
               fixedfee.brand = "1" and
               fixedfee.hosttable = "mobsub" and
               fixedfee.keyvalue = string(limsseq) and
               fixedfee.billcode begins "mdub" and
               fixedfee.begper = 201010 and
               fixedfee.begdate >= 10/1/10 and 
               fixedfee.begdate <= 10/31/10 and
               fixedfee.amt = 8,
         first ffitem of fixedfee exclusive-lock where
               ffitem.billed = false and
               ffitem.billperiod = 201010:
      

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhfixedfee).
         assign
            fixedfee.endper = 201009
            fixedfee.memo[2] = "Fee removed due to delayed creation.". 
         delete ffitem.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhfixedfee).
         k = k + 1.
         leave.
      end.
   end.
   
   pause 0.
   disp i j k with 1 down.

end.

fCleanEventObjects().


   


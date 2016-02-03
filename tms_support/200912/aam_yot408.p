{Syst/testpaa.i}
katun = "ari".

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPLMN AS HANDLE NO-UNDO.
   lhPLMN = BUFFER PLMN:HANDLE.
   RUN StarEventInitialize(lhPLMN).
END.


def stream sread.
input stream sread from /apps/snet/200912/plmn_codes_v1.1.txt.

def var lcline as char no-undo.
def var lcplmn as char no-undo.
def var lcname as char no-undo.
def var lccountry as char no-undo.
def var lcprefix as char no-undo.
def var lcisoco as char no-undo.
def var i as int no-undo.

def buffer bplmn for plmn.

repeat:

   import stream sread unformatted lcline.
   
   assign
      lcplmn = entry(1,lcline,chr(9))
      lcname = entry(2,lcline,chr(9))
      lcprefix = entry(3,lcline,chr(9))
      lccountry = entry(4,lcline,chr(9)).
      
   if lcprefix = "ccode" then next.
   
   find first plmn where plmn.plmn = lcplmn and 
      plmn.countryprefix = lcprefix no-lock no-error.
   if available plmn then next.
   
   find first plmn where plmn.plmn = lcplmn no-lock no-error.
   if available plmn and plmn.countryprefix = "" and lcprefix = "1"
   then next.
   
   i = i + 1.
   disp 
      lcplmn format "x(6)"
      lcname format "x(20)"
      lcprefix format "x(5)"
      lccountry format "x(25)"
      available plmn
      can-find(first plmn where plmn.countryprefix = lcprefix).
      
   if not available plmn then do:
   
      if lcprefix = "1" then lcprefix = "".

      lcisoco = "".
      find first country where country.coname = lccountry no-lock no-error.
      if available country then lcisoco = country.country.
      else do:
         find first bplmn where bplmn.coname = lccountry no-lock no-error.
         if available bplmn then lcisoco = bplmn.country.
         else if lcprefix > "" then do:
            find first bplmn where bplmn.countryprefix = lcprefix 
               no-lock no-error.
            if available bplmn then lcisoco = bplmn.country.
         end.

         if lcisoco = "" then do:
            case lccountry:
            when "usa" then lcisoco = "US".
            when "montenegro" then lcisoco = "CS".
            otherwise do:
               message "country:" lccountry
               view-as alert-box.
            end.
            end case.
         end.
      end.
      
      if lcisoco = "" then next.
      
      disp lcisoco format "x(3)".
      
      create plmn.
      assign 
         plmn.plmn = lcplmn
         plmn.commname = lcname
         plmn.country = lcisoco
         plmn.coname  = lccountry
         plmn.countryprefix = lcprefix.
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPLMN).

   end.
   
end.

disp i.

input stream sread close.

fcleaneventobjects().



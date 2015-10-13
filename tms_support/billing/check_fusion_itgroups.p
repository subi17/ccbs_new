def buffer binvoicetarget for invoicetarget.
def buffer bmobsub for mobsub.
def buffer btermmobsub for termmobsub.
def buffer binvoicetargetgroup for invoicetargetgroup.
DEFINE VARIABLE lCLiType AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcCLITypes AS CHARACTER NO-UNDO init "contsf,contff".
DEFINE VARIABLE lcCLiType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liLoop AS INTEGER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 

DEFINE VARIABLE lcLogFile AS CHARACTER NO-UNDO. 
lcLogFile = "/apps/yoigo/tms_support/billing/log/check_fusion_itgroups_" + 
   string(YEAR(TODAY) * 100 + MONTH(TODAY)) + ".txt".

def stream sout.
output stream sout to value(lcLogFile).

DEFINE VARIABLE lcCheck AS CHARACTER NO-UNDO format "x(40)". 
def frame a lccheck label "Check" i label "Count" j label "Errors" with 5 down
   title " Fusion invoice target group check. YPR-887 ".
lcCheck = "checking invoice target groups..".

put stream sout unformatted 
   "IT_GROUP;IT_DEL_TYPE;CUSTNUM;MSSEQ;MSSIDN;CLITYPE;ERROR" skip.

DEFINE VARIABLE k AS INTEGER NO-UNDO. 
DEFINE VARIABLE llFound AS LOGICAL NO-UNDO. 

disp lcCheck with frame a.
ITG_LOOP:
FOR EACH invoicetargetgroup NO-LOCK /* where
         invoicetargetgroup.itgroupid > 2400000 */ :

   i = i + 1.

   if i mod 1000 = 0 then do:
      disp i j with frame a.
      pause 0.
   end.

   if deltype eq 3 or deltype eq 13 then i = i + 1.
   else next.

   k = 0.
   llFound = false.
   FOR EACH invoicetarget NO-LOCK where
            invoicetarget.itgroupid = invoicetargetgroup.itgroupid and
            invoicetarget.todate > today:

      llFound = true.

      FIND FIRST mobsub NO-LOCK where
                 mobsub.msseq = invoicetarget.msseq no-error.

      IF AVAIL mobsub then do: 
         if (not mobsub.clitype begins "contsf" and
             not mobsub.clitype begins "contff") then do:
            j = j + 1.
           put stream sout unformatted 
           invoicetargetgroup.itgroupid ";" 
           invoicetargetgroup.deltype ";"
           mobsub.custnum ";"
           mobsub.msseq ";"
           mobsub.cli ";"
           mobsub.clitype ";ERROR:active non-fusion subscriptions in fusion invoice target group" skip. 
            next ITG_LOOP.
         end.
         k = k + 1.
      end.

      FIND FIRST termmobsub NO-LOCK where
                 termmobsub.msseq = invoicetarget.msseq no-error.
      IF AVAIL termmobsub then do: 
         if (not termmobsub.clitype begins "contsf" and
             not termmobsub.clitype begins "contff") then do:
           put stream sout unformatted 
           invoicetargetgroup.itgroupid ";" 
           invoicetargetgroup.deltype ";"
           termmobsub.custnum ";"
           termmobsub.msseq ";"
           termmobsub.cli ";"
           termmobsub.clitype ";ERROR:terminated non-fusion subscriptions in fusion invoice target group" skip. 
         j = j + 1.
         next ITG_LOOP.
         end.
      end.
   end.

   if k > 1 then do:
           put stream sout unformatted 
           invoicetargetgroup.itgroupid ";" 
           invoicetargetgroup.deltype ";"
           invoicetargetgroup.custnum ";"
           ";"
           ";"
           ";WARNING:Fusion invoice target group contains " k " active fusion subscriptions" skip. 
       j = j + 1.
       next ITG_LOOP.
   end.
      
   if invoicetargetgroup.defaultgroup then  do:
        put stream sout unformatted 
      invoicetargetgroup.itgroupid ";"
      invoicetargetgroup.deltype ";"
      invoicetargetgroup.custnum ";"
      ";"
      ";"
      ";"
      ";ERROR: Fusion invoice target group set as default group" skip.
       j = j + 1.
       next ITG_LOOP.
   end.
   
   k = 0.
   if llFound then
   FOR EACH binvoicetargetgroup NO-LOCK where
            binvoicetargetgroup.custnum = invoicetargetgroup.custnum and
            rowid(binvoicetargetgroup) ne rowid(invoicetargetgroup):

       if binvoicetargetgroup.deltype ne 3 and
          binvoicetargetgroup.deltype ne 13 then next.

       FOR EACH invoicetarget NO-LOCK where
                invoicetarget.itgroupid = binvoicetargetgroup.itgroupid and
                invoicetarget.todate > today:
         FIND FIRST mobsub NO-LOCK where
                   mobsub.msseq = invoicetarget.msseq no-error.
         IF AVAIL mobsub then k = k + 1.
       end.
   end.

   if k > 0 then do:
       put stream sout unformatted 
      invoicetargetgroup.itgroupid ";"
      invoicetargetgroup.deltype ";"
      invoicetargetgroup.custnum ";"
      ";"
      ";"
      ";"
      ";ERROR: Customer has more than one fusion invoice target group with active fusion subscrptions" skip.
       j = j + 1.
       next ITG_LOOP.
   end.

end.

down 1 with frame a.
lcCheck = "checking active fusion tariffs..".
disp lcCheck with frame a.
do liLoop = 1 to 2:
FOR EACH mobsub NO-LOCK where
         mobsub.brand = "1" and
         mobsub.clitype begins entry(liLoop,lcCLiTypes): 
   RUN pCheckSubs((BUFFER mobsub:handle)).
END.
END.

down 1 with frame a.
lcCheck =  "checking terminated fusion tariffs..".
disp lcCheck with frame a.
do liLoop = 1 to 2:
FOR EACH termmobsub NO-LOCK where
         termmobsub.brand = "1" and
         termmobsub.clitype begins entry(liLoop,lcCLiTypes): 
   RUN pCheckSubs((BUFFER termmobsub:handle)).
END.
END.

PROCEDURE pCheckSubs:

   DEFINE INPUT PARAMETER ihMobsub AS HANDLE NO-UNDO. 
/*
       FIND FIRST msrequest NO-LOCK where
                  msrequest.msseq = mobsub.msseq and
                  msrequest.reqtype = 0 and
                  msrequest.reqstatus = 2 and
                  msrequest.actstamp = 20131101 and
                  msrequest.reqcparam2 = mobsub.clitype no-error. 
       IF AVAIL msrequest then next.
   */    
      FIND invoicetarget NO-LOCK where
           invoicetarget.msseq = ihMobsub::msseq and
           invoicetarget.todate > today no-error.
      IF NOT AVAIL invoicetarget THEN DO:
         put stream sout unformatted
         ";"
         ";"
         ihMobsub::custnum ";"
         ihMobsub::msseq  ";"
         ihMobsub::CLI ";"
         ihMobsub::CLIType
         ";ERROR: active/terminated fusion subscription without active non-unique invoice target" skip.
         j = j + 1.
        next. 
      END.

      find invoicetargetgroup NO-LOCK where
          invoicetargetgroup.itgroupid = invoicetarget.itgroupid.

      if invoicetargetgroup.deltype ne 3 and
         invoicetargetgroup.deltype ne 13 then do:
         put stream sout unformatted
         invoicetargetgroup.itgroupid ";"
         invoicetargetgroup.deltype ";"
         invoicetargetgroup.custnum ";"
         invoicetarget.msseq  ";"
         ihMobsub::CLI ";"
         ihMobsub::CLIType
         ";ERROR: fusion subscription in non-fusion invoice target group" skip.
         j = j + 1.
      end.

      i = i + 1.
END PROCEDURE. 

MESSAGE j "errors found. Log file created to" lcLogFile VIEW-AS ALERT-BOX.

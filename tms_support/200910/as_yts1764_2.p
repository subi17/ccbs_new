DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE liLimitPlace AS INTEGER NO-UNDO. 

def stream slog.
output stream slog to /apps/snet/200910/as_yts1764_2_1.log.
DEFINE BUFFER bLimit FOR LIMIT.

FIX_LOOP:
FOR EACH limit where
   limit.brand = "" and
   limit.limittype = 2  NO-LOCK:

   find first eventlog NO-LOCK where
       eventlog.tablename = "limit" and
       eventlog.key begins string(limit.custnum) + "ÿ" 
                      + string(limit.limittype) + chr(255)  
                      + string(limit.tmruleseq) + chr(255) + 
                       string(limit.limitid)  + chr(255) + 
                      string(YEAR(limit.todate),"9999") + "/" +
                      string(MONTH(limit.todate),"99") + "/" +
                      string(DAY(limit.todate),"99")  NO-eRROR.
                      
     IF AVAIL eventlog and usercode ne "ari" then do:
     
     liLimitPlace = lookup("ToDate", eventlog.modifiedfields).

     if liLimitPlace > 0 and eventdate >= 10/21/2009 /*and
        entry((liLimitPlace - 1) * 3 + 3, eventlog.datavalues, chr(255)) = "<Null>" */
     then do:
     
        find first blimit where
         blimit.custnum = limit.custnum and
         blimit.limittype = 2 and
         recid(blimit) ne recid(limit) NO-LOCK NO-ERROR.

        if avail blimit then do:
            MESSAGE blimit.custnum VIEW-AS ALERT-BOX.
            next FIX_LOOP.
        end.
        
        /* check that only one evenlog exists */
        find eventlog NO-LOCK where
            eventlog.tablename = "limit" and
             eventlog.key begins string(limit.custnum) + "ÿ" 
                            + string(limit.limittype) + chr(255)  
               and eventlog.usercode ne "ari" NO-eRROR.
        if not avail eventlog then do:
            MESSAGE "not unique eventlog" VIEW-AS ALERT-BOX.
        /*   FOR EACH  eventlog NO-LOCK where
               eventlog.tablename = "limit" and
               eventlog.key begins string(limit.custnum) + "ÿ" 
               + string(limit.limittype) + chr(255):
               disp eventlog.
           end. */
            next FIX_LOOP.
        end.
        
        find blimit where recid(blimit) = recid(limit) EXCLUSIVE-LOCK.  
        
        put stream slog unformatted eventlog.eventdate  " "
           eventlog.eventtime " "
           eventlog.action " "
           limit.custnum " "
           limit.limitamt " "
           eventlog.action " "
           limit.fromdate " "
           limit.todate " "
           entry((liLimitPlace - 1) * 3 + 2, eventlog.datavalues, chr(255)) " "
           entry((liLimitPlace - 1) * 3 + 3, eventlog.datavalues, chr(255)) " " 
           recid(limit) " "
           recid(blimit) " "  
           recid(eventlog) " "
           eventlog.datavalues skip.

        blimit.todate = 12/31/2049.
        release blimit.

      end.
   end.
end.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE liLimitPlace AS INTEGER NO-UNDO. 

def stream slog.
output stream slog to /apps/snet/200910/as_yts1764_br_0.log.
DEFINE BUFFER bLimit FOR LIMIT.

FIX_LOOP:
FOR EACH limit where
   limit.brand = "" and
   limit.limittype = 2  NO-LOCK:

   find first eventlog NO-LOCK where
       eventlog.tablename = "limit" and
       eventlog.key eq string(limit.custnum) + "ÿ" 
                      + string(limit.limittype) + chr(255)  
                      + string(limit.tmruleseq) + chr(255) + 
                       string(limit.limitid)  + chr(255) + 
                      string(YEAR(limit.todate),"9999") + "/" +
                      string(MONTH(limit.todate),"99") + "/" +
                      string(DAY(limit.todate),"99") NO-eRROR.
                      
     IF AVAIL eventlog and usercode ne "ari" then do:
     
     liLimitPlace = lookup("LimitAmt", eventlog.modifiedfields).

     if liLimitPlace > 0 and eventdate >= 10/21/2009 and
        entry((liLimitPlace - 1) * 3 + 3, eventlog.datavalues, chr(255)) = "<Null>" then do:

        find first blimit where
         blimit.custnum = limit.custnum and
         blimit.limittype = 2 and
         recid(blimit) ne recid(limit) NO-LOCK NO-ERROR.

        /* should be handled seperately */
        IF AVAIL blimit then do:
            MESSAGE "other limit was found" blimit.custnum VIEW-AS ALERT-BOX.
            NEXT FIX_LOOP.
        end.
        ELSE IF limit.limitamt NE ? THEN DO:
            MESSAGE "limit is not unknown" limit.custnum limit.limitamt 
            VIEW-AS ALERT-BOX.
            NEXT FIX_LOOP.
        END.
        ELSE DO:
           /* check that only one evenlog exists */
           find eventlog NO-LOCK where
               eventlog.tablename = "limit" and
                eventlog.key begins string(limit.custnum) + "ÿ" 
                               + string(limit.limittype) + chr(255)  
                  and eventlog.usercode ne "ari" NO-eRROR.
           if not avail eventlog then do:
               MESSAGE "not unique eventlog" VIEW-AS ALERT-BOX.
              FOR EACH  eventlog NO-LOCK where
                  eventlog.tablename = "limit" and
                  eventlog.key begins string(limit.custnum) + "ÿ" 
                  + string(limit.limittype) + chr(255):
                  disp eventlog.
              end. 
               next FIX_LOOP.
           end.
        END.
        
        DEFINE VARIABLE liOldValue AS INTEGER NO-UNDO. 
        liOldValue = int(entry((liLimitPlace - 1) * 3 + 2, eventlog.datavalues, chr(255))).

        find blimit where recid(blimit) = recid(limit) EXCLUSIVE-LOCK.  
        
        put stream slog unformatted eventlog.eventdate  " "
           eventlog.eventtime " "
           eventlog.usercode " "
           limit.custnum " "
           limit.limitamt " "
           eventlog.action " "
           limit.fromdate "  "
           limit.todate " "
           liOldValue " "
           entry((liLimitPlace - 1) * 3 + 3, eventlog.datavalues, chr(255)) " " 
           recid(limit) " "
           recid(blimit) " "  
           recid(eventlog) skip.
          i = i + 1.
        
        blimit.limitamt = liOldValue.  
        release blimit.
      end.
   end.
end.

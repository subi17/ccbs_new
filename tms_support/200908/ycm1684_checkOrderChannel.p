
/* run it first only to display and after thant doing changes */
DEFINE STREAM sout.
DEFINE VARIABLE lcOutLine AS CHARACTER NO-UNDO. 

OUTPUT STREAM sout TO "order_ycm1684.log".

FOR EACH MsRequest NO-LOCK WHERE
         MSRequest.Brand = "1" AND
         MsRequest.ReqType = 0,
    FIRST Order EXCLUSIVE-LOCK WHERE
          Order.Brand = "1" AND
          Order.MSSeq = MsRequest.MsSeq AND
          Order.OrderChannel  =  "renewal_pos" :

    lcOutLine = STRING(Order.OrderId) + " " + STRING(MsRequest.MsRequest).
    
    /* all orders in statuscode 32,4,2 should be renewal_pos_stc from now ! */
    IF LOOKUP(Order.StatusCode,"32,4,2") > 0 OR
       LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0 THEN DO:
       
       PUT STREAM sout UNFORMATTED
          lcOutLine
        SKIP.

       ASSIGN Order.OrderChannel = "Renewal_POS_STC".


    END.


    RELEASE Order.  
END.

OUTPUT STREAM sout CLOSE.


/* postaddrb.p       11.04.05/aam

   customers' address updates from Posti  
*/


{Syst/commpaa.i}
gcBrand = "1".

{Func/lib/eventlog.i}
       
DEF VAR liQty   AS INT    NO-UNDO.
DEF VAR liDel   AS INT    NO-UNDO. 
DEF VAR lcError AS CHAR   NO-UNDO. 


/* new customers to post's address surveillance */
fELog("DAILY","NewCust2PostiStarted").

RUN Mc/postfilec (OUTPUT liQty,
               OUTPUT liDel,
               OUTPUT lcError).

fELog("POST","NewCust2PostiStopped:" + 
             STRING(liQty) + ":" +
             STRING(liDel) + 
             (IF lcError > "" THEN ":" + lcError ELSE "")).





/* SAPC-60 Customer has new field in TMS telling is the case PL or SAPC 

   Program: SAPC-60-check.p
   Date: 12/06/18

   Purpose: 
      We are using an existing field customer.accgrp for storing
    the PL (1) or SAPC (2) values. There is a risk that somebody could have the 
    same idea and re-use the field for his own purposes (even temporarily)
    or that for any reason any record has a different value than 1 (one).
   
      The purpose of this program is to check that the customer.accgrp
    field has only records with a 1 (one) value. This value is the default
    and it represents "PL". When moving SAPC project to production we need 
    to be sure all records have a 1 (one) in this field.
    
      In addition, the program can be used for monitoring the project during
    the first steps as it can provide the number of customers in SAPC mode 
    and dump them to a file
*/
    
&GLOBAL-DEFINE max_dump 20000

DEF VAR liOption AS INT  NO-UNDO.
DEF VAR lcFile   AS CHAR NO-UNDO FORMAT "X(60)".
DEF VAR liNumRec AS INT  NO-UNDO.


DEF STREAM strdump.
 
FORM
  SKIP  
  "  1) Check Customer.AccGrp = 1 for all records   "       SKIP
  "  2) Count number of customers in SAPC mode      "       SKIP
  "  3) Dump customers in SAPC mode to a file       "       SKIP
  "     File:" lcFile                                       SKIP
  "  4) Exit                                        "       SKIP(2) 
  "  Selection: " liOption  
WITH FRAME sapc60-menu NO-LABELS.

VIEW FRAME sapc60-menu.

blk:
REPEAT ON ERROR UNDO blk, LEAVE blk
       ON STOP  UNDO blk, LEAVE blk:
          
   lcFile = SESSION:TEMP-DIRECTORY + STRING(YEAR(TODAY),"9999") + 
                                     STRING(MONTH(TODAY),"99")  + 
                                     STRING(DAY(TODAY),"99")    + "-" + 
                                     STRING(TIME) + ".d".
                              
   UPDATE lcfile liOption WITH FRAME sapc60-menu.

   CASE liOption:
      WHEN 1 THEN
      DO:
         IF CAN-FIND(FIRST customer WHERE customer.accgrp <> 1 NO-LOCK) THEN
            MESSAGE "Bad news!" SKIP(1) 
                    "At least in one record customer.accgrp <> 1" 
               VIEW-AS ALERT-BOX ERROR.
         ELSE 
            MESSAGE "Good news!" SKIP(1) 
                    "All recors have customer.accgrp = 1" 
               VIEW-AS ALERT-BOX ERROR.
      END.
      WHEN 2 THEN
      DO:
         liNumRec = 0.
         FOR EACH customer FIELDS(custnum) WHERE customer.accgrp = 2 NO-LOCK:
            ASSIGN liNumRec = liNumRec + 1.
         END.
         MESSAGE "Number of customers in SAPC mode = " liNumRec 
           VIEW-AS ALERT-BOX INFORMATION.      
      END.
      WHEN 3 THEN
      DO:
         liNumRec = 0.
         OUTPUT STREAM strdump TO value(lcFile).
            FOR EACH customer NO-LOCK 
               WHERE customer.accgrp = 2 
                     liNumRec = 1 TO {&max_dump}:
               EXPORT STREAM strdump customer.
            END.
         OUTPUT STREAM strdump CLOSE.
         
         IF liNumrec = 0 THEN 
            MESSAGE "No records were dumped" 
              VIEW-AS ALERT-BOX INFORMATION.      
         ELSE
         IF liNumRec > {&max_dump} THEN /* Process was forced to leave loop */
            MESSAGE "Records dumped = " (liNumRec - 1) SKIP(1)
                    "Note: dump process limited to a maximum of " {&max_dump} "records" 
              VIEW-AS ALERT-BOX INFORMATION.      
         ELSE 
            MESSAGE "Records dumped = " liNumRec
              VIEW-AS ALERT-BOX INFORMATION.      
      END.
      WHEN 4 THEN QUIT.
      OTHERWISE 
         MESSAGE "Wrong option" VIEW-AS ALERT-BOX ERROR.
   END.
END.

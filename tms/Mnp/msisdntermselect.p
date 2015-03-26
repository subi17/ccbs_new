/* ----------------------------------------------------------------------
  MODULE .......: msisdntermselect.p
  TASK .........: Lists MSISDN termination statuses
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 09.12.09
  Version ......: xfera
----------------------------------------------------------------------- */
   
DEFINE VARIABLE lcSelected AS CHARACTER NO-UNDO.

RUN selectbox.p(
  "MSISDN NUMBER TERMINATION",
  "11 - WAITING RETURN TO ORIG. OPERATOR|14 - RETURN NOTICE SENT|12 - RETURNED TO ORIG. OPERATOR",
  OUTPUT lcSelected).

IF lcSelected NE "" AND lcSelected NE ? THEN DO:
  RUN msisdn.p("",0,int(entry(1,lcSelected,"-"))).
END.

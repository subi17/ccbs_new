/* ----------------------------------------------------------------------
  MODULE .......: MOBCALLi.P
  TASK .........: Browse  mobile calls
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-09-99
  CHANGED ......: jt 26.01.06 UserName = DYNAMIC-FUNCTION("fDispCustName" IN                                           ghFunc1, BUFFER Customer)
  Version ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{commali.i} 
{msisdn.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'mobcdr'}
{func.i}

DEF INPUT PARAMETER  CustNum AS INT NO-UNDO.

DEF VAR ocReasonCode                  AS C    NO-UNDO.
DEF VAR odtDate1                      AS DATE NO-UNDO.
DEF VAR odtDate2                      AS DATE NO-UNDO.
DEF VAR olAccept                      AS LOG  NO-UNDO. 

RUN  mobguard2.p(INPUT  TRUE,
               OUTPUT ocReasonCode,
               OUTPUT odtDate1,
               OUTPUT odtdate2,
               OUTPUT olAccept).

IF olAccept = FALSE THEN LEAVE.


RUN mobcallbr(INPUT "post,pre",
              INPUT odtDate1,
              INPUT odtDate2,
              INPUT Custnum,
              INPUT "inv",
              INPUT "", 
              INPUT 0,
              INPUT 0,
              INPUT "",
              INPUT "",
              INPUT ocReasonCode,
              INPUT 0,
              INPUT 0).


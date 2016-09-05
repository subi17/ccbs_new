/* ----------------------------------------------------------------------
  MODULE .......: MSCOUNT.P
  TASK .........: Count no. of MobSubs
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 03.08.99
  CHANGED ......: 01.12.99 pt CustNum filter
                  10.05.03 jp only active/in use status
                  11.09.03 jp Brand
                  04.05.05 mvi display result for all 38 statuscodes
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Mm/mobsub1.i}

DEF VAR amt1 AS i  NO-UNDO  EXTENT 50. /* all */
DEF VAR amt2 AS i  NO-UNDO  EXTENT 50. /* alive */
DEF VAR amt3 AS i  NO-UNDO  EXTENT 50. /* contract signed */

DEF VAR i   AS i  NO-UNDO.

DEF VAR CustNum LIKE Customer.CustNum NO-UNDO.

CustNum = 0.
repeat:
   message "Enter customer no:  (empty: ALL customers) " UPDATE CustNum.
   IF can-find(Customer where Customer.CustNum = CustNum) OR CustNum = 0
   THEN LEAVE.

   MESSAGE "UNKNOWN CUSTOMER" VIEW-AS ALERT-BOX.
   NEXT.
END.      
    

   FOR EACH MobSub WHERE Mobsub.Brand = gcBrand AND 
   (IF CustNum NE 0 THEN MobSub.CustNum = CustNum ELSE TRUE) no-lock:

      /* ALL */
      amt1[MobSub.MsStatus + 1] = amt1[MobSub.MsStatus + 1] + 1.

      /* alive */
      IF Mobsub.Msstatus >= 3 AND 
         Mobsub.MsStatus < 10 THEN 
      amt2[MobSub.MsStatus + 1] = amt2[MobSub.MsStatus + 1] + 1.

      /* contract signed */
      IF MobSub.Contract THEN
      amt3[MobSub.MsStatus + 1] = amt3[MobSub.MsStatus + 1] + 1.
   END.

  FOR EACH MSOwner no-lock where
           MSOwner.TsEnd   <= 999999999  AND
           MSOwner.Brand    = gcBrand    AND 
     NOT CAN-FIND(First mobsub WHERE mobsub.msseq = msowner.msseq)     . 

       ASSIGN
          amt1[16] = amt1[16] + 1.
   END.

   DO i = 1 TO 38:
      if amt1[i] = 0 then next. 
      DISP 
        i - 1 format "z9" label "SCode"
        entry(i,stnames)  label "Name of Status" format "x(30)"
        amt1[i]           label "TotAmt"
        amt2[i]           label "Alive"
        amt3[i]           label "CSigned"

      with 11 down centered row 3 title " Summary Of Mobile Subscriptions "
      OVERLAY FRAME Qty.
      DOWN WITH FRAME Qty.
   END.

   ASSIGN ufk = 0 ehto = 3. RUN Syst/ufkey.p.

   message "Press ENTER to continue !".
   PAUSE no-message.
   HIDE FRAME Qty no-pause.

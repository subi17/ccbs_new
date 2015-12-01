/* -----------------------------------------------
  MODULE .......: NNCOIT.P
  FUNCTION .....: CREATE contract invoice items
  APPLICATION ..: NN
  AUTHOR .......: pt
  CREATED ......: 01-12-98
  MODIFIED .....: 04.12.98 kl values FOR Concerns
  Version ......: M15
                  27.12.01 jp fcper NOT used
                  03.03.03 aam input parameters for fcheck-pl
                  24.06.03 aam iiItemQty for fMakeContract
                  09.09.03 aam brand 
                  12.03.04 jp  not full monthly fee
                  29.09.04 aam check iiItemQty also after broken month 
                  30.11.04 aam check for 00-period in broken month
                  30.12.04 aam make always separate item from broken month
                  25.02.05 aam allow earlier billing period than beg.period
                               with billing method 1 
                  09.12.05 aam test if 'concern' in fMakeContractMore is
                               only 6 digits long
  --------------------------------------------------------------------- */

&IF "{&NNCOIT2_I}" NE "YES"
&THEN
&GLOBAL-DEFINE NNCOIT2_I YES

{commali.i}

FUNCTION fCheck-pl RETURNS LOGICAL
   (icFeeModel AS CHAR,
    icPriceList AS CHAR).

   DEF VAR ok AS LO  NO-UNDO.
   ok = CAN-FIND (FIRST FMItem WHERE
                        FMItem.Brand     = gcBrand    AND
                        FMItem.FeeModel  = icFeeModel AND
                        FMItem.PriceList = icPriceList).
   IF NOT ok THEN
   MESSAGE 
   "Price List code on Customer's "                           SKIP
   "Billing Target is" icPriceList  
    SKIP
               "BUT"                                          SKIP
   "Billing Event" icFeeModel "does NOT contain"              SKIP
   "any Billable items for that Price List Code !"
   VIEW-AS ALERT-BOX WARNING.

   RETURN ok.
END.

FUNCTION fNextP RETURNS INTEGER
   (xPeriod AS INTEGER).
   DEF VAR yy AS INT NO-UNDO.
   DEF VAR mm AS INT NO-UNDO.


   /* calculate NEXT Period */
   yy = TRUNCATE(xPeriod / 100,0).
   mm = xPeriod - (yy * 100) + 1.
   IF mm  = 13 THEN ASSIGN yy = yy + 1 mm = 1.
   RETURN (yy * 100 + mm).
END.   


FUNCTION fMakeContract RETURN INT 
   (INPUT FFNum     AS INT,
    INPUT iiItemQty AS INT):

  DEF VAR rc    AS INT NO-UNDO.
  def var dD1   as da no-undo format "99-99-9999".
  def var dD2   as da no-undo format "99-99-9999".
  DEF VAR iTmp  AS i  NO-UNDO.
  DEF VAR yyyy     AS i  NO-UNDO.
  DEF VAR mm       AS i  NO-UNDO.
  DEF VAR per      AS i  NO-UNDO.
  DEF VAR expper   AS i  NO-UNDO.
  DEF VAR i        AS i  NO-UNDO.
  DEF VAR ddate    AS DA NO-UNDO.
  DEF VAR per3     AS I  NO-UNDO.
  DEF VAR debug    AS LO NO-UNDO.
  DEF VAR monthdates  AS I NO-UNDO.
  DEF VAR billedDATES AS I NO-UNDO.
  DEF VAR ldaEndDate AS DATE NO-UNDO. 

  DEF VAR liBrokenRental AS INT  NO-UNDO. 

  DEF BUFFER xFFItem FOR FFItem.
  DEF BUFFER brokenitem for ffitem.
  DEF BUFFER bFMITemBroken FOR FMItem.

  FIND FixedFee where FixedFee.FFNum = FFNum no-lock.

  /* on 'endless' contract we must DEFINE an absolute expiration Period */
  IF EndPeriod = 999999 THEN DO:
     ldaEndDate = ADD-INTERVAL(FixedFee.BegDate,5,"months").
     expper = year(ldaEndDate) * 100 + month(ldaEndDate).
  END.
  ELSE expper = FixedFee.EndPeriod.

   /* broken rental for 1. month or full */
  IF DAY(FixedFee.BegDate) NE 1 THEN DO:
     liBrokenRental = 9.
     FOR FIRST bFMITemBroken NO-LOCK WHERE
               bFMITemBroken.Brand      = gcBrand AND
               bFMITemBroken.FeeModel   = FixedFee.FeeModel AND
               bFMITemBroken.BillCode   = FixedFee.BillCode AND
               bFMITemBroken.BillMethod = FALSE:
        IF bFMITemBroken.FirstMonthBR > 0 THEN liBrokenRental = 1.       
     END.
  END.
  ELSE liBrokenRental = 0.
    
  IF liBrokenRental = 9 THEN DO:
    IF MONTH(FixedFee.BegDate) = 12
    THEN ddate = DATE(1,1,YEAR(FixedFee.BegDate) + 1).
    ELSE ddate = DATE(MONTH(FixedFee.BegDate) + 1,1,YEAR(FixedFee.BegDate)).
    ASSIGN
       billeddates = ddate - fixedfee.begdate
       monthdates  = DAY(ddate - 1).
  END.   

  ELSE ddate = FixedFee.BegDate.

  ASSIGN
  dD2    = ddate 
  per    = FixedFee.BegPeriod
  dd1    = dd2.
  
  DO WHILE YEAR(ddate) * 100 + MONTH(ddate) <= expper:

     iTmp =  FixedFee.Interval.
     DO WHILE iTmp > 0:
         dD2 = dD2 + 1.
         IF day(dD2) = 1 THEN iTmp = iTmp - 1.
     END.

     /* set activation Period */
     case FixedFee.BillMethod:
         when 1 THEN ASSIGN yyyy = year(ddate) mm = month(ddate) - 1.
         when 2 THEN ASSIGN yyyy = year(ddate) mm = month(ddate).
         when 3 THEN ASSIGN yyyy = year(ddate) 
                            mm   = month(ddate) + FixedFee.Interval + 1. 
     END case.

     IF mm >= 13 THEN ASSIGN yyyy  = yyyy + 1 mm = mm - 12 .
     IF mm = 0   THEN ASSIGN yyyy  = yyyy - 1 mm = 12.
     per3 = yyyy * 100 + mm.


     /* NOT 1st DAY of a MONTH */
     IF NOT day(dD1) = 1 AND liBrokenRental NE 1 THEN DO:
        ASSIGN iTMP = month(dD2).
        /* loop: until dates match - WHILE months match */

        repeat:
        IF day(dD2) < day(FixedFee.BegDate) - 1 AND month(dD2 + 1) = iTmp 
           THEN dD2 = dD2 + 1.
           ELSE LEAVE.
        END.
        /* NOT 1st DAY of MONTH => Period can't END LAST DAY of MONTH */
        IF month(dD2 + 1) NE month(dD2) THEN dD2 = dD2 - 1.
     END.
     ELSE dD2 = dD2 - 1.


     /****************************************
     * Now we CREATE a NEW monthly Billable  *
     * item                                  *
     ****************************************/

     DO FOR xFFItem  TRANS:

        CREATE xFFItem.
        ASSIGN
        xFFItem.FFNum      = FixedFee.FFNum
        xFFItem.FFItemNum  = next-value(citem)
        xFFItem.BillPeriod = per
        xFFItem.Amt        = FixedFee.Amt
        xFFItem.OwnCost    = FixedFee.OwnCost
        xFFItem.Billed     = FALSE
        xFFItem.InvNum     = 0
        xFFItem.BillCode   = FixedFee.BillCode
        xFFItem.CustNum    = FixedFee.CustNum
        rc                 = rc + 1.

        DO i = 1 TO 5.
           xFFItem.Memo[i] = FixedFee.Memo[i].
        END.

        ASSIGN
        xFFItem.Concerns[1] = year(dd1) * 10000 + month(dd1) * 100 + day(dd1) 
        xFFItem.Concerns[2] = year(dd2) * 10000 + month(dd2) * 100 + day(dd2)

        xFFItem.BillPeriod  = IF per3 < year(FixedFee.BegDate) * 100 + 
                                        month(FixedFee.BegDate) AND
                                 FixedFee.BillMethod NE 1 
                              THEN year(FixedFee.BegDate) * 100 +
                                   month(FixedFee.BegDate)                  
                              ELSE per3.

        /* max number of items defined */
        IF iiItemQty > 0 AND rc = iiItemQty THEN DO:

           /* update fee to end to last created item */
           FIND CURRENT FixedFee EXCLUSIVE-LOCK.
           FixedFee.EndPeriod = YEAR(ddate) * 100 + MONTH(ddate).

           LEAVE.
        END.

        IF rc > 50 THEN LEAVE.

        ASSIGN dD1    = dD2 + 1 
               ddate  = dd1
               dd2    = dd1 .

     END.
   END.

   IF liBrokenRental = 9 THEN DO TRANS:

      FIND FIRST ffitem of fixedfee no-lock no-error.
      CREATE BrokenItem  .
      BUFFER-COPY ffitem except ffitemnum to BrokenItem.
      ASSIGN
         BrokenItem.FFNum       = FixedFee.FFNum
         BrokenItem.FFItemNum   = next-value(citem)
         BrokenItem.BillPeriod  = FFItem.BillPeriod - 1
         BrokenItem.Amt         = FixedFee.Amt / monthdates * billeddates
         BrokenItem.Concerns[1] = year(FixedFee.BegDate)  * 10000 + 
                                  month(Fixedfee.BegDate) * 100   + 
                                  day(FixedFee.Begdate) 
         BrokenItem.Concerns[2] = 
            year(FixedFee.BegDate + billeddates - 1) * 10000 + 
            month(Fixedfee.BegDate + billeddates - 1) * 100 +
            day(FixedFee.Begdate + billeddates - 1).

      /* year change */
      IF BrokenItem.BillPeriod MOD 100 = 0 
      THEN BrokenItem.BillPeriod = (TRUNCATE(FFItem.BillPeriod / 100,0) - 1)
                                   * 100 + 12.
         
      /* if broken month's amt is atleast the normal fee -> add it to 
         item qty */
      IF ABS(BrokenItem.Amt) >= ABS(FixedFee.Amt) THEN rc = rc + 1.

      FIND CURRENT FixedFee EXCLUSIVE-LOCK.

      /* max number of items defined and broken month exceeds it 
         -> delete last item */
      IF iiItemQty > 0 AND rc > iiItemQty THEN DO:

          FIND LAST FFItem OF FixedFee EXCLUSIVE-LOCK.
          DELETE FFItem.
          rc = rc - 1.

          /* update fee to end to last created item */
          FIND LAST FFItem OF FixedFee NO-LOCK.
          FixedFee.EndPeriod = TRUNCATE(FFItem.Concerns[2] / 100,0).

      END.
      FixedFee.CalcAmt = BrokenItem.Amt.
      FIND CURRENT FixedFee NO-LOCK.
   END.

  RETURN rc.
END.

FUNCTION fMakeContractMore RETURN INT 
(INPUT FFNum AS INT,
 INPUT concern AS INTEGER):

  DEF VAR rc    AS INT NO-UNDO.
  def var dD1   as da no-undo format "99-99-9999".
  def var dD2   as da no-undo format "99-99-9999".
  DEF VAR iTmp  AS i  NO-UNDO.
  DEF VAR yyyy     AS i  NO-UNDO.
  DEF VAR mm       AS i  NO-UNDO.
  DEF VAR per      AS i  NO-UNDO.
  DEF VAR expper   AS i  NO-UNDO.
  DEF VAR i        AS i  NO-UNDO.
  DEF VAR ddate    AS DA NO-UNDO.
  DEF VAR per3     AS I  NO-UNDO.
  DEF VAR debug    AS LO NO-UNDO.
  DEF VAR startdate as da no-undo.
  DEF VAR ldaEndDate AS DATE NO-UNDO. 

  DEF BUFFER xFFItem FOR FFItem.

  IF concern > 999999 THEN DO:
      startdate = date(INT(SUBSTRING(string(concern),5,2)),
                       INT(SUBSTRING(string(concern),7,2)), 
                       INT(SUBSTRING(string(concern),1,4))).
      startdate = startdate + 1.
  END.
                 
  ELSE DO:
     startdate = date(INT(SUBSTRING(string(concern),5,2)),
                      1, 
                      INT(SUBSTRING(string(concern),1,4))).
     IF MONTH(startdate) = 12 
     THEN startdate = DATE(1,1,YEAR(startdate) + 1).
     ELSE startdate = DATE(MONTH(startdate) + 1,1,YEAR(startdate)).
  END. 
                   
  FIND FixedFee where FixedFee.FFNum = FFNum no-lock.

  /* on 'endless' contract we must DEFINE an absolute expiration Period */
  IF EndPeriod = 999999 THEN DO:
     ldaEndDate = ADD-INTERVAL(startdate,5,"months").
     expper = year(ldaEndDate) * 100 + month(ldaEndDate).
  END.
  ELSE expper = endperiod.

  ASSIGN
  per    = FixedFee.BegPeriod
  ddate  = startdate
  dD2    = startdate
  dd1    = dd2.

  DO WHILE YEAR(ddate) * 100 + MONTH(ddate) <= expper:

     iTmp =  FixedFee.Interval.
     DO WHILE iTmp > 0:
         dD2 = dD2 + 1.
         IF day(dD2) = 1 THEN iTmp = iTmp - 1.
     END.

     /* set activation Period */
     case FixedFee.BillMethod:
         when 1 THEN ASSIGN yyyy = year(ddate) mm = month(ddate) - 1.
         when 2 THEN ASSIGN yyyy = year(ddate) mm = month(ddate).
         when 3 THEN ASSIGN yyyy = year(ddate) 
                            mm   = month(ddate) + FixedFee.Interval + 1.
     END case.

     IF mm >= 13 THEN ASSIGN yyyy  = yyyy + 1 mm = mm - 12 .
     IF mm = 0   THEN ASSIGN yyyy  = yyyy - 1 mm = 12.
     per3 = yyyy * 100 + mm.


     /* NOT 1st DAY of a MONTH */
     IF NOT day(dD1) = 1 THEN DO:
        ASSIGN iTMP = month(dD2).
        /* loop: until dates match - WHILE months match */

        repeat:
        IF day(dD2) < day(FixedFee.BegDate) - 1 AND month(dD2 + 1) = iTmp 
           THEN dD2 = dD2 + 1.
           ELSE LEAVE.
        END.
        /* NOT 1st DAY of MONTH => Period can't END LAST DAY of MONTH */
        IF month(dD2 + 1) NE month(dD2) THEN dD2 = dD2 - 1.
     END.
     ELSE dD2 = dD2 - 1.


     /****************************************
     * Now we CREATE a NEW monthly Billable  *
     * item                                  *
     ****************************************/

     DO FOR xFFItem  TRANS:

        CREATE xFFItem.
        ASSIGN
        xFFItem.FFNum     = FixedFee.FFNum
        xFFItem.FFItemNum     = next-value(citem)
        xFFItem.BillPeriod  = per
        xFFItem.Amt    = FixedFee.Amt
        xFFItem.OwnCost   = FixedFee.OwnCost
        xFFItem.Billed     = FALSE
        xFFItem.InvNum      = 0
        xFFItem.BillCode  = FixedFee.BillCode
        xFFItem.CustNum    = FixedFee.CustNum
        rc                    = rc + 1.
        DO i = 1 TO 5.
           xFFItem.Memo[i] = FixedFee.Memo[i].
        END.

        ASSIGN
        xFFItem.Concerns[1] = year(dd1) * 10000 + month(dd1) * 100 + day(dd1) 
        xFFItem.Concerns[2] = year(dd2) * 10000 + month(dd2) * 100 + day(dd2)

        xFFItem.BillPeriod     = IF per3 < year(FixedFee.BegDate) * 100 + 
                                          month(FixedFee.BegDate) 
                                 THEN year(FixedFee.BegDate) * 100 +
                                      month(FixedFee.BegDate)
                                 ELSE per3
        dD1                   = dD2 + 1 
        ddate                 = dd1
        dd2                   = dd1 .
        IF rc > 50 THEN LEAVE.
     END.
  END.

  RETURN rc.
END.

&ENDIF

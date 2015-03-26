/* ------------------------------------------------------
  MODULE .......: itsendco.p
  FUNCTION .....: collect customers for sending information texts via eMail
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.05.03
  MODIFIED .....: 12.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF TEMP-TABLE ttCustGroup NO-UNDO
   FIELD CustGroup AS CHAR
   INDEX CustGroup CustGroup.

DEF TEMP-TABLE ttCGMember NO-UNDO
   FIELD CustNum AS INT
   INDEX CustNum CustNum.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT. 

DEF INPUT  PARAMETER icInvGrp   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER TABLE FOR ttCustGroup.
DEF INPUT  PARAMETER iiCustNum1 AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiCustNum2 AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER TABLE FOR ttCust. 


EMPTY TEMP-TABLE ttCust.

/* external group members */
FOR EACH ttCustGroup,
    EACH CGMember NO-LOCK WHERE
         CGMember.Brand     = gcBrand AND
         CGMember.custgroup = ttCustGroup.CustGroup:

   IF CAN-FIND(FIRST ttCGMember WHERE 
                     ttCGMember.CustNum = CGMember.CustNum)
   THEN NEXT.

   CREATE ttCGMember.
   ttCGMember.CustNum = CGMember.CustNum.
END.

/* get customers with different ways according to given criterias */ 
IF CAN-FIND(FIRST ttCGMember) THEN 
FOR EACH ttCGMember:

   IF ttCGMember.CustNum < iiCustNum1 OR 
      ttCGMember.CustNum > iiCustNum2 
   THEN NEXT.

   IF icInvGrp NE "" THEN DO:
      FIND Customer WHERE 
           Customer.CustNum = ttCGMember.CustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer OR Customer.InvGroup NE icInvGrp 
      THEN NEXT.
   END.

   CREATE ttCust.
   ttCust.CustNum = ttCGMember.CustNum.

END.

ELSE IF icInvGrp NE "" THEN
FOR EACH Customer NO-LOCK WHERE
         Customer.Brand    = gcBrand    AND
         Customer.InvGroup = icInvGrp   AND
         Customer.CustNum >= iiCustNum1 AND
         Customer.CustNum <= iiCustNum2:

   CREATE ttCust.
   ttCust.CustNum = Customer.CustNum.
END.

ELSE 
FOR EACH Customer NO-LOCK WHERE
         Customer.Brand    = gcBrand    AND
         Customer.CustNum >= iiCustNum1 AND
         Customer.CustNum <= iiCustNum2:

   CREATE ttCust.
   ttCust.CustNum = Customer.CustNum.
END.



&IF "{&forderstamp}" NE "YES" 
&THEN

&GLOBAL-DEFINE forderstamp YES
/* forderstamp.i      20.11.06/aam

   mark and read order time stamps 
*/
{commali.i}
{timestamp.i}
{tmsconst.i}


/* change possible descriptive type to an integer key */
FUNCTION fOrderStampType RETURNS INTEGER
   (icStampType AS CHAR):

   CASE icStampType:

   WHEN "Change" OR
   WHEN "Ch"     OR
   WHEN "1"  THEN RETURN {&ORDERTIMESTAMP_CHANGE}.
   
   WHEN "Delivery" OR
   WHEN "Del"      OR
   WHEN "2"  THEN RETURN {&ORDERTIMESTAMP_DELIVERY}.

   WHEN "Close" OR
   WHEN "Cl"    OR
   WHEN "3"  THEN RETURN {&ORDERTIMESTAMP_CLOSE}.

   WHEN "Print" OR
   WHEN "Pr"    OR
   WHEN "4"  THEN RETURN {&ORDERTIMESTAMP_PRINT}.
   
   WHEN "SimOnly" OR
   WHEN "Si"    OR
   WHEN "5"  THEN RETURN {&ORDERTIMESTAMP_SIMONLY}.

   WHEN "SendToLogistics" OR
   WHEN "Se"    OR
   WHEN "6"  THEN RETURN {&ORDERTIMESTAMP_SEND}.

   OTHERWISE RETURN 0.
   
   END CASE. 
   
END FUNCTION.

/* mark a stamp value */
FUNCTION fMarkOrderStamp RETURNS LOGICAL
   (iiOrderID   AS INT,
    icStampType AS CHAR,
    idStamp     AS DEC):
 
   DEF VAR liStampType AS INT NO-UNDO.
       
   liStampType = fOrderStampType(icStampType).
   
   IF liStampType = 0 THEN RETURN FALSE.
   
   FIND FIRST OrderTimeStamp WHERE
              OrderTimeStamp.Brand   = gcBrand   AND
              OrderTimeStamp.OrderID = iiOrderID AND
              OrderTimeStamp.RowType = liStampType EXCLUSIVE-LOCK NO-ERROR.
              
   IF NOT AVAILABLE OrderTimeStamp THEN DO:
      CREATE OrderTimeStamp.
      ASSIGN OrderTimeStamp.Brand   = gcBrand
             OrderTimeStamp.OrderID = iiOrderID 
             OrderTimeStamp.RowType = liStampType.
   END. 

   /* use current time if not given */ 
   IF idStamp = 0 THEN idStamp = fMakeTS().
   
   OrderTimeStamp.TimeStamp = idStamp.    

   RELEASE OrderTimeStamp.
   
END FUNCTION.

FUNCTION fGetOrderStamp RETURNS DECIMAL
   (iiOrderID   AS INT,
    icStampType AS CHAR):

   DEF VAR liStampType AS INT NO-UNDO.
       
   liStampType = fOrderStampType(icStampType).
   
   IF liStampType = 0 THEN RETURN 0.0.
   
   FIND FIRST OrderTimeStamp WHERE
              OrderTimeStamp.Brand   = gcBrand   AND
              OrderTimeStamp.OrderID = iiOrderID AND
              OrderTimeStamp.RowType = liStampType NO-LOCK NO-ERROR.
              
   IF AVAILABLE OrderTimeStamp 
   THEN RETURN OrderTimeStamp.TimeStamp.
   ELSE RETURN 0.0.
     
END FUNCTION.

&ENDIF

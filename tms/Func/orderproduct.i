&IF "{&ORDERPRODUCT_ID}" NE "YES"
&THEN
&GLOBAL-DEFINE ORDERPRODUCT_ID YES

FUNCTION fGetOPParamDate RETURNS DATE
   (INPUT iiOrderProductID AS INT,
    INPUT icParamName AS CHAR):

   DEF BUFFER OrderProductParam FOR OrderProductParam.

   FIND OrderProductParam NO-LOCK WHERE
        OrderProductParam.OrderProductID = iiOrderProductID AND
        OrderProductParam.ParamName = icParamName NO-ERROR. 
   IF NOT AVAIL OrderProductParam THEN RETURN ?.

   IF AVAIL OrderProductParam THEN RETURN 
      OrderProductParam.DateValue.
   ELSE RETURN ?.

END.

&ENDIF

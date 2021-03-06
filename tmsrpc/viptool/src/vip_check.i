DEF VAR lcVIPError AS CHARACTER NO-UNDO. 
DEF BUFFER vipMSISDN FOR MSISDN.

FIND FIRST vipMSISDN NO-LOCK WHERE 
   vipMSISDN.brand EQ Syst.Var:gcBrand AND
   vipMSISDN.cli EQ MobSub.Cli USE-INDEX CLI NO-ERROR.

IF NOT AVAIL vipMSISDN THEN lcVIPError = "Msisdn was not found".
ELSE IF vipMSISDN.POS NE "VIP" THEN lcVIPError = "Msisdn is not a VIP number".

IF lcVIPError NE "" THEN DO:
   RETURN appl_err(lcVIPError).
END.

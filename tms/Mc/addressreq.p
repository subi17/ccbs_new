/* ----------------------------------------------------------------------------
  MODULE .......: addressreq.p
  FUNCTION .....: customer address handling
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 12.09.07 (separated from msrequest.i)
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */
{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Func/fcustdata.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEFINE VARIABLE lcRegion AS CHARACTER NO-UNDO.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 6 THEN RETURN "ERROR".

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
END.


RUN pAddressChange.

fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pAddressChange:

   DEF BUFFER bACC FOR MsRequest.

   DEF VAR lcStreetCode AS CHAR NO-UNDO. 
   DEF VAR lcCityCode AS CHAR NO-UNDO. 
   DEF VAR lcTownCode AS CHAR NO-UNDO. 
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   lcRegion = STRING(MsRequest.ReqIParam2,"99").
   
   IF lcRegion ne "00" THEN 
   FIND FIRST Region WHERE Region.Region = lcRegion NO-LOCK NO-ERROR.
   IF NOT AVAIL Region THEN DO:
      fReqError("Unknown region " + lcRegion).
      RETURN.
   END.

   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Brand   = gcBrand AND
            MobSub.AgrCust = MsRequest.CustNum,
      FIRST bACC NO-LOCK WHERE
            bACC.MsSeq   = MobSub.MsSeq AND
            bACC.ReqType = 10           AND
            LOOKUP(STRING(bACC.ReqStatus),"2,4,9") = 0:
      fReqError("Pending ACC on subscription " + STRING(MobSub.MsSeq)).
      RETURN.
   END.
    
   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum
      EXCLUSIVE-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).
   
   ASSIGN 
          Customer.Address    = MsRequest.ReqCParam1
          Customer.PostOffice = MsRequest.ReqCParam2
          Customer.Country    = MsRequest.ReqCParam3
          Customer.COName     = MsRequest.ReqCParam4 
            WHEN MsRequest.ReqSource NE "6"
          Customer.ZipCode    = STRING(MsRequest.ReqIParam1,"99999")
          Customer.Region     = lcRegion
          Customer.InvGroup    = fDefInvGroup(lcRegion).
   
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).

   ASSIGN
       lcStreetCode = (IF MsRequest.ReqIParam3 NE 0 THEN 
         STRING(MsRequest.ReqIParam3) ELSE "")
       lcCityCode = (IF MsRequest.ReqIParam4 NE 0 THEN
         STRING(MsRequest.ReqIParam4) ELSE "")
       lcTownCode = (IF MsRequest.ReqDParam1 NE 0 THEN
         STRING(INT(MsRequest.ReqDParam1)) ELSE "").

   FIND FIRST CustomerReport WHERE
              CustomerReport.Custnum = Customer.Custnum
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL CustomerReport AND
      (lcStreetCode > "" OR 
       lcCityCode > "" OR
       lcTownCode > "") THEN CREATE CustomerReport.

   IF AVAIL CustomerReport THEN
   ASSIGN
       CustomerReport.Custnum = Customer.Custnum
       CustomerReport.StreetCode = lcStreetCode
       CustomerReport.CityCode = lcCityCode
       CustomerReport.TownCode = lcTownCode.
   
   RELEASE Customer.
   RELEASE CustomerReport.
             
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.


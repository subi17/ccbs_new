{Syst/commpaa.i}
ASSIGN
   Syst.Var:katun = "cron"
   Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}

DEFINE TEMP-TABLE ttReport NO-UNDO
   FIELD name AS CHAR
   FIELD type AS CHAR
   FIELD field_value AS CHAR
INDEX name IS PRIMARY UNIQUE name.

DEF OUTPUT PARAM TABLE FOR ttReport.

DEFINE VARIABLE ldeBegin AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeEnd   AS DECIMAL NO-UNDO.  

/* RESULTS */
DEFINE VARIABLE liOrdersYesterday AS INTEGER NO-UNDO. 
DEFINE VARIABLE liOrdersToday AS INTEGER NO-UNDO. 
DEFINE VARIABLE liActiveSubs AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMNPOngoing AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttSales NO-UNDO
   FIELD name AS CHAR
   FIELD count AS INT
   FIELD saledate as date
   INDEX name IS PRIMARY UNIQUE name saledate.

FUNCTION fSales RETURNS LOGICAL
   (ldSaleDate AS DATE):

   FIND FIRST SalesMan NO-LOCK WHERE
              SalesMan.Brand = Syst.Var:gcBrand AND
              SalesMan.SalesMan = Order.SalesMan NO-ERROR.

   IF AVAIL SalesMan THEN DO:
            
      FIND FIRST ttSales WHERE
                 ttSales.name = SalesMan.Reseller and
                 ttSales.saledate = ldSaleDate EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ttSales THEN DO:
         ttSales.count = ttSales.count + 1.
      END.
      ELSE DO:
         CREATE ttSales.
         ASSIGN
            ttSales.name = Salesman.Reseller
            ttSales.saledate = ldSaleDate
            ttSales.count = 1.
      END.
   END.
   ELSE DO:
         
      FIND FIRST ttSales WHERE
                 ttSales.name = Order.Salesman and
                 ttSales.saledate = ldSaleDate EXCLUSIVE-LOCK NO-ERROR.
      
      IF AVAIL ttSales THEN assign
         ttSales.count = ttSales.count + 1.
      ELSE DO:
         CREATE ttSales.
         ASSIGN
            ttSales.name = Order.Salesman
            ttSales.saledate = ldSaleDate
            ttSales.count = 1.
      END.
   
   END.

END FUNCTION. 

FUNCTION fReport RETURNS LOGICAL
(  icName  AS CHAR,
   icType AS CHAR,
   icValue AS CHAR):
   CREATE ttReport.
   ASSIGN 
      ttReport.name = icName 
      ttReport.type = icType
      ttReport.field_value = icValue.

END FUNCTION. 

FUNCTION fReportInt RETURNS LOGICAL
(iiName AS CHAR,
iiValue AS INTEGER):
   fReport(iiName, "int", STRING(iiValue)).
END FUNCTION. 

FUNCTION fReportChar RETURNS LOGICAL
(iiName AS CHAR,
icValue AS CHAR):
   fReport(iiName, "char", icValue).
END FUNCTION. 

FUNCTION fFixNeg RETURNS INT 
(iiValue AS INTEGER):
   IF iiValue > 0 THEN RETURN iiValue.
   ELSE RETURN 0.
END FUNCTION. 

/* Today Orders */
ldeBegin = Func.Common:mMake2DT(TODAY,0).
DEFINE VARIABLE liPrepaidsToday AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPostpaidsToday AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPosSales AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCCSales AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTeleSales AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRenewal AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRenewalTelesales AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRenewalPos AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRenewalPosStc AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRetention AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSelfSales AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTerminalsToday AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSIMsToday AS INTEGER NO-UNDO. 
DEFINE VARIABLE li3GsToday AS INTEGER NO-UNDO. 

DEFINE VARIABLE liTotalMNPIn AS INTEGER NO-UNDO.
DEFINE VARIABLE liOrangeIn AS INTEGER NO-UNDO.
DEFINE VARIABLE liVodafoneIn AS INTEGER NO-UNDO.
DEFINE VARIABLE liMovistar AS INTEGER NO-UNDO.
DEFINE VARIABLE liOthersIn AS INTEGER NO-UNDO.

DEFINE VARIABLE liMGMSalesToday AS INTEGER NO-UNDO. 

/* Today Orders */
FOR EACH Order WHERE
         Order.Brand = Syst.Var:gcBrand AND
         Order.CrStamp > ldeBegin AND
         Order.OrderType <= {&ORDER_TYPE_STC} NO-LOCK:

   FIND FIRST OrderAccessory OF Order NO-LOCK WHERE
              OrderAccessory.ProductCode ne "" AND
              OrderAccessory.TerminalType EQ ({&TERMINAL_TYPE_PHONE}) NO-ERROR.
   
   IF AVAIL OrderAccessory THEN
      liTerminalsToday = liTerminalsToday + 1.

   IF Order.ICC ne "" THEN
      liSimsToday = liSimsToday + 1.
   
   IF (INDEX(Order.CLIType,"contrd") > 0 OR
       Order.CLIType eq "tarjrd1") AND
      AVAIL OrderAccessory THEN
      li3GsToday = li3GsToday + 1.
   
   liOrdersToday = liOrdersToday + 1.
   CASE Order.OrderChannel: 
      WHEN "pos" OR WHEN "fusion_pos" OR WHEN "fusion_pos_pro" OR WHEN "pos_pro" then 
         liPosSales = liPosSales + 1.
      WHEN "cc" OR WHEN "fusion_cc" then liCCSales = liCCSales + 1.
      WHEN "telesales" OR WHEN "fusion_telesales" OR 
          WHEN "telesales_pro" OR WHEN "fusion_telesales_pro" then liTeleSales = liTeleSales + 1.
      WHEN "self" OR WHEN "fusion_self" then liSelfSales = liSelfSales + 1.
      WHEN "renewal" then liRenewal = liRenewal + 1.
      WHEN "renewal_telesales" or
      WHEN "renewal_ctc" then liRenewalTelesales = liRenewalTelesales  + 1.
      WHEN "renewal_pos" then liRenewalPos = liRenewalPos + 1.
      WHEN "renewal_pos_stc" then liRenewalPosStc = liRenewalPosStc + 1.
      WHEN "retention" then liRetention = liRetention + 1.
   END.

   IF Order.MNPStatus > 0 THEN DO:
      IF Order.curroper BEGINS "Orange" THEN liOrangeIn = liOrangeIn + 1.
      ELSE IF Order.curroper EQ "Vodafone" THEN liVodafoneIn = liVodafoneIn + 1.
      ELSE IF Order.curroper EQ "Movistar" THEN liMovistar = liMovistar + 1.
      ELSE liOthersIn = liOthersIn + 1.
      
      liTotalMNPIn = liTotalMNPIn + 1.
   END.

   IF Order.PayType THEN liPrepaidsToday = liPrepaidsToday + 1.
   ELSE liPostpaidsToday = liPostpaidsToday + 1.

   IF Order.OrderType NE 2 AND
      Order.OrderType NE 4 THEN fSales(TODAY).

   IF Order.Referee ne "" THEN
      liMGMSalesToday = liMGMSalesToday + 1.

END.

/* Yesterday Orders */
ldeBegin = Func.Common:mMake2DT(TODAY - 1,0).
ldeEnd   = Func.Common:mMake2DT(TODAY,0).
DEFINE VARIABLE liMGMSalesYesterday AS INTEGER NO-UNDO. 

FOR EACH Order WHERE
         Order.Brand = Syst.Var:gcBrand AND
         Order.CrStamp >= ldeBegin AND
         Order.CrStamp < ldeEnd AND
         Order.OrderType <= {&ORDER_TYPE_STC} NO-LOCK:

   liOrdersYesterday = liOrdersYesterday + 1.

   IF Order.OrderType NE 2 AND
      Order.OrderType NE 4 THEN fSales(TODAY - 1).

   IF Order.Referee ne "" THEN
      liMGMSalesYesterday = liMGMSalesYesterDay + 1.

END.

/* Cumulative prepaid / postpaid sales CPU */
DEFINE VARIABLE liPrepaids AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPostpaids AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPosSalesCum AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCCSalesCum AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTeleSalesCum AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSelfSalesCum AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMGMSalesCum AS INTEGER NO-UNDO. 

FOR EACH Order NO-LOCK WHERE
         Order.Brand = Syst.Var:gcBrand AND
         Order.OrderType <= {&ORDER_TYPE_STC}:

   IF Order.PayType THEN
   liPrepaids = liPrepaids + 1.
   ELSE
   liPostpaids = liPostpaids + 1.
   
   CASE Order.OrderChannel: 
      WHEN "pos" OR WHEN "fusion_pos" OR WHEN "pos_pro" 
         OR WHEN "fusion_pos_pro" then liPosSalesCum = liPosSalesCum + 1.
      WHEN "cc" OR WHEN "fusion_cc" then liCCSalesCum = liCCSalesCum + 1.
      WHEN "telesales" OR WHEN "fusion_telesales"
         OR WHEN "telesales_pro" OR WHEN "fusion_telesales_pro" then liTeleSalesCum = liTeleSalesCum + 1.
      WHEN "self" OR WHEN "fusion_self" then liSelfSalesCum = liSelfSalesCum + 1.
   END.
   
   IF Order.Referee ne "" THEN
      liMGMSalesCum = liMGMSalesCum + 1.

END.

/* Actual Subscriptions (HLR) */
FOR EACH mobsub NO-LOCK: 
   liActiveSubs = liActiveSubs + 1.
END.

/* Total Ongoing MNPs */
FOR EACH Order WHERE
         Order.Brand = Syst.Var:gcBrand AND
         Order.StatusCode = "12" AND
         Order.MNPStatus > 0 NO-LOCK:
   liMNPOngoing = liMNPOngoing + 1. 
END.

DEFINE VARIABLE liMNPOutOrange    AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMNPOutMovistar  AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMNPOutVodafone  AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMNPOutOthers    AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMNPOutTotal     AS INTEGER NO-UNDO. 

DEFINE VARIABLE liOpNumber AS INTEGER NO-UNDO. 

/* Today MNP Out */
ldeBegin = Func.Common:mMake2DT(TODAY,0).
ldeEnd   = Func.Common:mMake2DT(TODAY + 1,0).

FOR EACH msrequest where
         msrequest.brand = Syst.Var:gcBrand and 
         msrequest.reqtype = 18 and
         msrequest.reqstatus = 2 and
         msrequest.actstamp >= ldeBegin and
         msrequest.actstamp < ldeEnd and
         reqcparam2 ne "" NO-LOCK:
  
   liOpNumber = int(msrequest.reqcparam2).
   
   if liOpNumber = 711111 then
      liMNPOutMovistar = liMNPOutMovistar + 1.
   else if 
      liOpNumber >= 730000 and 
      liOpNumber <= 734999 then 
      liMNPOutOrange = liMNPOutOrange + 1.
   else if 
      liOpNumber >= 720000 and
      liOpNumber <= 724999 then 
      liMNPOutVodafone = liMNPOutVodafone + 1.
   else 
      liMNPOutOthers = liMNPOutOthers + 1.

   liMNPOutTotal = liMNPOutTotal + 1.

END.

FUNCTION fTermSubs RETURNS INT 
(idaDate AS DATE):

   DEFINE VARIABLE ldeBegin AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE ldeEnd   AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE i        AS INTEGER NO-UNDO. 

   ldeBegin = Func.Common:mMake2DT(idaDate,0).
   ldeEnd   = Func.Common:mMake2DT(idaDate + 1,0).
   
   FOR EACH MsRequest WHERE 
            MsRequest.Brand = Syst.Var:gcBrand AND
            MsRequest.ReqType = 18 AND
            MsRequest.ReqStatus = 0 AND
            MsRequest.ActStamp >= ldeBegin AND 
            MsRequest.ActStamp < ldeEnd NO-LOCK:
      i = i + 1.
   END.

   FOR EACH MsRequest WHERE 
            MsRequest.Brand = Syst.Var:gcBrand AND
            MsRequest.ReqType = 18 AND
            MsRequest.ReqStatus = 2 AND
            MsRequest.ActStamp >= ldeBegin AND 
            MsRequest.ActStamp < ldeEnd NO-LOCK:
      i = i + 1.
   END.

   RETURN i.

END FUNCTION. 


ldeBegin = Func.Common:mMake2DT(DATE(MONTH(TODAY),1,YEAR(TODAY)),0).
DEFINE VARIABLE liPrivatePrepaidNew AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPrivatePrepaidMNP AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPrivatePostpaidNew AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPrivatePostpaidMNP AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCorporatePrepaidNew AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCorporatePrepaidMNP AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCorporatePostpaidNew AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCorporatePostpaidMNP AS INTEGER NO-UNDO. 

FOR EACH Order WHERE
         Order.Brand = Syst.Var:gcBrand AND
         Order.CrStamp >= ldeBegin NO-LOCK:

   IF Order.OrderType EQ {&ORDER_TYPE_ACC} THEN NEXT.
   
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
   
   CASE Order.PayType:
      /* prepaid */
      WHEN TRUE THEN DO:
         
         IF Order.MNPStatus = 0 THEN DO:
            IF OrderCustomer.CustidType = "CIF" 
               THEN liCorporatePrepaidNew = liCorporatePrepaidNew + 1.
               ELSE liPrivatePrepaidNew  = liPrivatePrePaidNew + 1.
         END.
         ELSE DO:
            IF OrderCustomer.CustidType = "CIF" 
               THEN liCorporatePrepaidMNP = liCorporatePrepaidMNP + 1.
               ELSE liPrivatePrepaidMNP  = liPrivatePrePaidMNP + 1.
         END.
      
      END.
      /* postpaid */
      WHEN FALSE THEN DO:
         IF Order.MNPStatus = 0 THEN DO:
            IF OrderCustomer.CustidType = "CIF" 
               THEN liCorporatePostpaidNew = liCorporatePostpaidNew + 1.
               ELSE liPrivatePostpaidNew  = liPrivatePostPaidNew + 1.
         END.
         ELSE DO:
            IF OrderCustomer.CustidType = "CIF" 
               THEN liCorporatePostpaidMNP = liCorporatePostpaidMNP + 1.
               ELSE liPrivatePostpaidMNP  = liPrivatePostPaidMNP + 1.
         END.
      END.
   END.
END. 


/* 1 */
fReportInt("t_orders", liOrdersToday).
fReportInt("y_orders", liOrdersYesterDay).
fReportInt("total_ong_mnps", liMNPOngoing).
fReportInt("active_subs", liActiveSubs).
fReportInt("t_terminated", fTermSubs(TODAY)).
fReportInt("y_terminated", fTermSubs(TODAY - 1)).

/* 2 & 3 */
FUNCTION fRepSales RETURNS LOGICAL (idaDay AS DATE):

   DEFINE VARIABLE liWEBSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liTSSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liTPHSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liTCSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liSMSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liBYSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liKHSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liTPSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liEXSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liTotSales AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcDay AS CHARACTER NO-UNDO.

   IF idaDay = TODAY THEN lcDay = "t_sales_".
   ELSE IF idaDay = TODAY - 1 THEN lcDay = "y_sales_".

   FOR EACH ttSales WHERE saledate = idaDay NO-LOCK:
      fReportInt(lcDay + ttSales.name, ttSales.count).
      IF lookup(ttSales.name, "WEB,MGM") > 0 THEN liWebSales = liWebSales + ttSales.count.
      IF lookup(ttSales.name, "RC,CT,YC,UN,GM,RB,EU,BE,CL,EM,CR") > 0 THEN liTSSales = liTSSales + ttSales.count.
      IF lookup(ttSales.name, "PH") > 0 THEN liTPHSales = liTPHSales + ttSales.count.
      IF lookup(ttSales.name, "TC") > 0 THEN liTCSales = liTCSales + ttSales.count.
      IF lookup(ttSales.name, "DX,MD") > 0 THEN liSMSales = liSMSales + ttSales.count.
      IF lookup(ttSales.name, "BY,AX,TL") > 0 THEN liBYSales = liBYSales + ttSales.count.
      IF lookup(ttSales.name, "KH,TA") > 0 THEN liKHSales = liKHSales + ttSales.count.
      IF lookup(ttSales.name, "TP,TU") > 0 THEN liTPSales = liTPSales + ttSales.count.
      IF lookup(ttSales.name, "DX,MD,BY,AX,TL,KH,TA,TP,TU") > 0 THEN liEXSales = liExSales + ttSales.count.
      IF lookup(ttSales.name, "WEB,MGM,RC,CT,YC,UN,GM,RB,EU,BE,CL,EM,CR,PH,TC,DX,MD,BY,AX,TL,KH,TA,TP,TU") > 0
         THEN liTotSales = liTotSales + ttSales.count.
   END.

   fReportInt(lcDay + "WEB_sum", liWEBSales).
   fReportInt(lcDay + "TS_sum", liTSSales).
   fReportInt(lcDay + "TPH_sum", liTPHSales).
   fReportInt(lcDay + "TC_sum", liTCSales).
   fReportInt(lcDay + "SM_sum", liSMSales).
   fReportInt(lcDay + "BY_sum", liBYSales).
   fReportInt(lcDay + "KH_sum", liKHSales).
   fReportInt(lcDay + "TP_sum", liTPSales).
   fReportInt(lcDay + "EX_sum", liEXSales).
   fReportInt(lcDay + "TOT_sum", liTOTSales).

END FUNCTION.

fRepSales(today - 1).
fRepSales(today).

DEFINE VARIABLE lcStatuses AS CHARACTER NO-UNDO.

lcStatuses = "req_&1_new,req_&1_underwork,req_&1_done,req_&1_rejected," +
             "req_&1_cancelled,req_&1_pending_hlr,req_&1_pending_hlr_done,,," +
             "req_&1_handled".



/* 4 & 5*/
FOR
   EACH MsReqCounter NO-LOCK WHERE
      MsReqCounter.Reqtype = 1 AND
      MsReqCounter.ReqStatus <= 9
   BREAK BY MsReqCounter.ReqStatus:

   ACCUMULATE MsReqCounter.ReqStatusCount (SUB-TOTAL BY MsReqCounter.ReqStatus).

   IF LAST-OF(MsReqCounter.ReqStatus) AND ENTRY(MsReqCounter.ReqStatus + 1,lcStatuses) > ""
   THEN fReportInt(SUBSTITUTE(ENTRY(MsReqCounter.ReqStatus + 1,lcStatuses),"service"), fFixNeg((ACCUM SUB-TOTAL BY MsReqCounter.ReqStatus MsReqCounter.ReqStatusCount))).
END.


FOR
   EACH MsReqCounter NO-LOCK WHERE
      MsReqCounter.Reqtype = 13 AND
      MsReqCounter.ReqStatus <= 9
   BREAK BY MsReqCounter.ReqStatus:

   ACCUMULATE MsReqCounter.ReqStatusCount (SUB-TOTAL BY MsReqCounter.ReqStatus).

   IF LAST-OF(MsReqCounter.ReqStatus) AND ENTRY(MsReqCounter.ReqStatus + 1,lcStatuses) > ""
   THEN fReportInt(SUBSTITUTE(ENTRY(MsReqCounter.ReqStatus + 1,lcStatuses),"sub"), fFixNeg((ACCUM SUB-TOTAL BY MsReqCounter.ReqStatus MsReqCounter.ReqStatusCount))).
END.

/* 6 not exists */

/* 7 */
fReportInt("t_ch_pos",liPosSales).
fReportInt("t_ch_cc",liCCSales).
fReportInt("t_ch_telesales",liTeleSales).
fReportInt("t_ch_renewal",liRenewal).
fReportInt("t_ch_renewal_telesales",liRenewalTelesales).
fReportInt("t_ch_renewal_pos",liRenewalPos).
fReportInt("t_ch_renewal_pos_stc",liRenewalPosStc).
fReportInt("t_ch_retention",liRetention).
fReportInt("t_ch_self",liSelfSales).
fReportInt("total_ch_pos",liPosSalesCum).
fReportInt("total_ch_cc",liCCSalesCum).
fReportInt("total_ch_telesales",liTeleSalesCum).
fReportInt("total_ch_self",liSelfSalesCum).

/* 8 */
fReportInt("y_prepaid_sales",liPrepaidsToday).
fReportInt("y_postpaid_sales",liPostpaidsToday).
fReportInt("total_postpaid_sales",liPostpaids).
fReportInt("total_prepaid_sales",liPrepaids).

/* 9 */
fReportInt("t_terminals",liTerminalsToday).
fReportInt("t_sims",liSimsToday).
fReportInt("t_3gmodems",li3GsToday).

/* 10 */
fReportInt("t_mnp_in_total", liTotalMNPIn).
fReportInt("t_mnp_in_movistar", liMovistar).
fReportInt("t_mnp_in_vodafone", liVodafoneIn).
fReportInt("t_mnp_in_orange", liOrangeIn).
fReportInt("t_mnp_in_MVNO", liOthersIn).

fReportInt("t_mnp_out_total", liMNPOutTotal).
fReportInt("t_mnp_out_movistar", liMNPOutMovistar).
fReportInt("t_mnp_out_vodafone", liMNPOutVodafone).
fReportInt("t_mnp_out_orange", liMNPOutOrange).
fReportInt("t_mnp_out_MVNO", liMNPOutOthers).

/* 11 not counted */
fReportInt("private_prepaid_new", liPrivatePrepaidNew).
fReportInt("private_prepaid_mnp", liPrivatePrepaidMNP).
fReportInt("private_postpaid_new", liPrivatePostPaidNew).
fReportInt("private_postpaid_mnp", liPrivatePostPaidMNP).
fReportInt("corporate_prepaid_new", licorporatePrepaidNew).
fReportInt("corporate_prepaid_mnp", licorporatePrepaidMNP).
fReportInt("corporate_postpaid_new", licorporatePostPaidNew).
fReportInt("corporate_postpaid_mnp", licorporatePostPaidMNP).

/* 12 */
fReportInt("t_plus1_terminated", fTermSubs(TODAY + 1)).
fReportInt("t_plus2_terminated", fTermSubs(TODAY + 2)).
fReportInt("t_plus3_terminated", fTermSubs(TODAY + 3)).
fReportInt("t_plus4_terminated", fTermSubs(TODAY + 4)).
fReportInt("t_plus5_terminated", fTermSubs(TODAY + 5)).

/* MGM */
fReportInt("t_mgm_sales", liMGMSalesToday).
fReportInt("y_mgm_sales", liMGMSalesYesterday).
fReportInt("total_mgm_sales", liMGMSalesCum).


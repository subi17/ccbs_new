Table: 


checkChar(   ""                                ,                                ,
             gcExpectedLanguage).


Table: Memo


checkChar(   "Memo.Brand"    ,Memo.Brand    ,gcExpectedBrand).
checkChar(   "Memo.CreUser"  ,Memo.CreUser  ,gcExpectedCreUser).
checkChar(   "Memo.CreUser"  ,Memo.CreUser  ,gcExpectedCreUser2).
checkChar(   "Memo.HostTable",Memo.HostTable,gcExpectedHostTable).
checkChar(   "Memo.KeyValue" ,Memo.KeyValue ,gcExpectedKeyValue).
checkChar(   "Memo.MemoSeq"  ,Memo.MemoSeq  ,gcExpectedMemoSeq).
checkChar(   "Memo.MemoText" ,Memo.MemoText ,gcExpectedMemoText2).
checkChar(   "Memo.MemoText" ,Memo.MemoText ,gcExpectedMemoText3).
checkChar(   "Memo.MemoTitle",Memo.MemoTitle,gcExpectedMemoTitle).
checkChar(   "Memo.MemoTitle",Memo.MemoTitle,gcExpectedMemoTitle2).
checkChar(   "Memo.MemoTitle",Memo.MemoTitle,gcExpectedMemoTitle3).
checkChar(   "Memo.KeyValue" ,Memo.KeyValue ,gcExpectedOrderID).
checkDecimal("Memo.CreStamp" ,Memo.CreStamp ,gdeExpectedCreStamp).


Table: MSISDN


checkChar(   "MSISDN.statuscode",MSISDN.statuscode,gcExpectedMsisdnStatusCode).
checkInt(    "msisdn.statuscode",msisdn.statuscode,giExpectedStatusCode).


Table: Order


checkChar(   "Order.Brand"        ,Order.Brand        ,gcExpectedBrand).
checkChar(   "Order.OrderChannel" ,Order.OrderChannel ,gcExpectedChannel).
checkChar(   "Order.CLI"          ,Order.CLI          ,gcExpectedCLI).
checkChar(   "Order.ContractID"   ,Order.ContractID   ,gcExpectedContractID).
checkChar(   "Order.DeviceID"     ,Order.DeviceID     ,gcExpectedDeviceId).
checkChar(   "Order.DeviceID"     ,Order.DeviceID     ,gcExpectedDeviceModel).
checkChar(   "Order.FeeModel"     ,Order.FeeModel     ,gcExpectedFeeModel).
checkChar(   "Order.OrdererId"    ,Order.OrdererId    ,gcExpectedOrdererID).
checkChar(   "Order.OrdererIdType",Order.OrdererIdType,gcExpectedOrdererIdType).
checkChar(   "Order.OrdererIp"    ,Order.OrdererIp    ,gcExpectedOrdererIp).
checkChar(   "Order.Source"       ,Order.Source       ,gcExpectedSource).
checkChar(   "Order.statuscode"   ,Order.statuscode   ,gcExpectedStatusCode).
checkDecimal("Order.CrStamp"      ,Order.CrStamp      ,gdeExpectedCreStamp).
checkInt(    "Order.InvCustRole"  ,Order.InvCustRole  ,giExpectedInvCustrole).
checkInt(    "Order.mnpstatus"    ,Order.mnpstatus    ,giExpectedMnpStatus).
checkInt(    "Order.msseq"        ,Order.msseq        ,giExpectedMsSeq).
checkInt(    "Order.OrderID"      ,Order.OrderID      ,giExpectedOrderId).
checkInt(    "Order.UserRole"     ,Order.UserRole     ,giExpectedUserRole).
checkLogical("Order.OldPayType"   ,Order.OldPayType   ,
             glExpectedOldPayType,'glExpectedOldPayType did not match.').
checkLogical("Order.Paytype"      ,Order.Paytype      ,
             glExpectedPayType,'glExpectedPayType did not match.').


Table: OrderAccessory


checkChar(   "OrderAccessory.Brand"      ,OrderAccessory.Brand      ,
             gcExpectedBrand).
checkChar(   "OrderAccessory.ProductCode",OrderAccessory.ProductCode,
             gcExpectedDeviceModel).
checkChar(   "OrderAccessory.IMEI"       ,OrderAccessory.IMEI       ,
             gcExpectedIMEI).
checkDecimal("OrderAccessory.Amount"     ,OrderAccessory.Amount     ,
             gdeExpectedDevicePrice).
checkInt(    "OrderAccessory.OrderID"    ,OrderAccessory.OrderID    ,
             giExpectedOrderId).


Table: OrderCustomer


checkChar(   "OrderCustomer.Address"           ,OrderCustomer.Address           ,
             gcExpectedAddress).
checkChar(   "OrderCustomer.PostOffice"        ,OrderCustomer.PostOffice        ,
             gcExpectedAddressCity).
checkChar(   "OrderCustomer.Address"           ,OrderCustomer.Address           ,
             gcExpectedAddressStreet).
checkChar(   "OrderCustomer.ZipCode"           ,OrderCustomer.ZipCode           ,
             gcExpectedAddressZip).
checkChar(   "OrderCustomer.Brand"             ,OrderCustomer.Brand             ,
             gcExpectedBrand).
checkChar(   "OrderCustomer.CustID"            ,OrderCustomer.CustID            ,
             gcExpectedCustID).
checkChar(   "OrderCustomer.CustIDType"        ,OrderCustomer.CustIDType        ,
             gcExpectedCustIDType).
checkChar(   "OrderCustomer.Language"          ,OrderCustomer.Language          ,
             gcExpectedLanguage).
checkChar(   "OrderCustomer.PostOffice"        ,OrderCustomer.PostOffice        ,
             gcExpectedPostOffice).
checkChar(   "OrderCustomer.Language"          ,OrderCustomer.Language          ,
             gcExpectedSecOrdCustLanguage).
checkChar(   "OrderCustomer.Nationality"       ,OrderCustomer.Nationality       ,
             gcExpectedSecOrdCustNational).
checkChar(   "OrderCustomer.Country"           ,OrderCustomer.Country           ,
             gcExpectedSecOrdCustrCountry).
checkChar(   "OrderCustomer.SurName"           ,OrderCustomer.SurName           ,
             gcExpectedSurName).
checkChar(   "OrderCustomer.ZIP"               ,OrderCustomer.ZIP               ,
             gcExpectedZipCode).
checkInt(    "OrderCustomer.RowType"           ,OrderCustomer.RowType           ,
             giExpectedRowType).
checkLogical("OrderCustomer.OperAllMarketing"  ,OrderCustomer.OperAllMarketing  ,
             glExpectedOperAllMarketing,'glExpectedOperAllMarketing did not match.').


Table: OrderPayment


checkChar(   "OrderPayment.Brand"      ,OrderPayment.Brand      ,
             gcExpectedBrand).
checkInt(    "OrderPayment.Method"     ,OrderPayment.Method     ,
             giExpectedMethod).


Table: OrderService


checkChar(   "OrderService.Brand"  ,OrderService.Brand  ,gcExpectedBrand).
checkInt(    "OrderService.OrderID",OrderService.OrderID,giExpectedOrderId).


Table: OrderTopup


checkInt(    "OrderTopup.OrderID",OrderTopup.OrderID,giExpectedOrderId).


Table: SIM


checkInt(    "SIM.simstat",SIM.simstat,giExpectedSIMStatus).



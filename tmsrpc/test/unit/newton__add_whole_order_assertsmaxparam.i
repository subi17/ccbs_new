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
checkChar(   "Memo.MemoText" ,Memo.MemoText ,gcExpectedMemoText).
checkChar(   "Memo.MemoText" ,Memo.MemoText ,gcExpectedMemoText2).
checkChar(   "Memo.MemoText" ,Memo.MemoText ,gcExpectedMemoText3).
checkChar(   "Memo.MemoTitle",Memo.MemoTitle,gcExpectedMemoTitle).
checkChar(   "Memo.MemoTitle",Memo.MemoTitle,gcExpectedMemoTitle2).
checkChar(   "Memo.MemoTitle",Memo.MemoTitle,gcExpectedMemoTitle3).
checkChar(   "Memo.KeyValue" ,Memo.KeyValue ,gcExpectedOrderID).
checkChar(   "Memo.CreUser"  ,Memo.CreUser  ,gcExpectedSalesman).
checkDecimal("Memo.CreStamp" ,Memo.CreStamp ,gdeExpectedCreStamp).


Table: MSISDN


checkChar(   "MSISDN.statuscode",MSISDN.statuscode,gcExpectedMsisdnStatusCode).
checkInt(    "msisdn.statuscode",msisdn.statuscode,giExpectedStatusCode).


Table: Order


checkChar(   "Order.Brand"        ,Order.Brand        ,gcExpectedBrand).
checkChar(   "Order.Campaign"     ,Order.Campaign     ,gcExpectedCampaign).
checkChar(   "Order.OrderChannel" ,Order.OrderChannel ,gcExpectedChannel).
checkChar(   "Order.CLI"          ,Order.CLI          ,gcExpectedCLI).
checkChar(   "Order.ContractID"   ,Order.ContractID   ,gcExpectedContractID).
checkChar(   "Order.CurrOper"     ,Order.CurrOper     ,gcExpectedCurrOper).
checkChar(   "Order.DeviceID"     ,Order.DeviceID     ,gcExpectedDeviceId).
checkChar(   "Order.DeviceID"     ,Order.DeviceID     ,gcExpectedDeviceModel).
checkChar(   "Order.FeeModel"     ,Order.FeeModel     ,gcExpectedFeeModel).
checkChar(   "Order.ICC"          ,Order.ICC          ,gcExpectedICC).
checkChar(   "Order.OldICC"       ,Order.OldICC       ,gcExpectedOldICC).
checkChar(   "Order.Orderer"      ,Order.Orderer      ,gcExpectedOrderer).
checkChar(   "Order.OrdererId"    ,Order.OrdererId    ,gcExpectedOrdererID).
checkChar(   "Order.OrdererIdType",Order.OrdererIdType,gcExpectedOrdererIdType).
checkChar(   "Order.OrdererIp"    ,Order.OrdererIp    ,gcExpectedOrdererIp).
checkChar(   "Order.Source"       ,Order.Source       ,gcExpectedSource).
checkChar(   "Order.statuscode"   ,Order.statuscode   ,gcExpectedStatusCode).
checkDecimal("Order.CrStamp"      ,Order.CrStamp      ,gdeExpectedCreStamp).
checkDecimal("Order.FAT"          ,Order.FAT          ,gdeExpectedFAT).
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
checkDecimal("OrderAccessory.discount"   ,OrderAccessory.discount   ,
             gdeExpectedDiscount).
checkInt(    "OrderAccessory.OrderID"    ,OrderAccessory.OrderID    ,
             giExpectedOrderId).


Table: OrderCustomer


checkChar(   "OrderCustomer.BankCode"          ,OrderCustomer.BankCode          ,
             gcExpectedAccount).
checkChar(   "OrderCustomer.Address"           ,OrderCustomer.Address           ,
             gcExpectedAddress).
checkChar(   "OrderCustomer.PostOffice"        ,OrderCustomer.PostOffice        ,
             gcExpectedAddressCity).
checkChar(   "OrderCustomer.CodC"              ,OrderCustomer.CodC              ,
             gcExpectedAddressCodC).
checkChar(   "OrderCustomer.CodP"              ,OrderCustomer.CodP              ,
             gcExpectedAddressCodP).
checkChar(   "OrderCustomer.FirstName"         ,OrderCustomer.FirstName         ,
             gcExpectedAddressFName).
checkChar(   "OrderCustomer.SurName1"          ,OrderCustomer.SurName1          ,
             gcExpectedAddressLName).
checkChar(   "OrderCustomer.SurName2"          ,OrderCustomer.SurName2          ,
             gcExpectedAddressLName2).
checkChar(   "OrderCustomer.Address"           ,OrderCustomer.Address           ,
             gcExpectedAddressStreet).
checkChar(   "OrderCustomer.ZipCode"           ,OrderCustomer.ZipCode           ,
             gcExpectedAddressZip).
checkChar(   "OrderCustomer.Brand"             ,OrderCustomer.Brand             ,
             gcExpectedBrand).
checkChar(   "OrderCustomer.CodC"              ,OrderCustomer.CodC              ,
             gcExpectedCodC).
checkChar(   "OrderCustomer.CodP"              ,OrderCustomer.CodP              ,
             gcExpectedCodP).
checkChar(   "OrderCustomer.Company"           ,OrderCustomer.Company           ,
             gcExpectedCompany).
checkChar(   "OrderCustomer.CustID"            ,OrderCustomer.CustID            ,
             gcExpectedCompanyId).
checkChar(   "OrderCustomer.SurName2"          ,OrderCustomer.SurName2          ,
             gcExpectedCoName).
checkChar(   "OrderCustomer.country"           ,OrderCustomer.country           ,
             gcExpectedCountry).
checkChar(   "OrderCustomer.CustID"            ,OrderCustomer.CustID            ,
             gcExpectedCustID).
checkChar(   "OrderCustomer.CustIDType"        ,OrderCustomer.CustIDType        ,
             gcExpectedCustIDType).
checkChar(   "OrderCustomer.email"             ,OrderCustomer.email             ,
             gcExpectedEmail).
checkChar(   "OrderCustomer.FixedNumber"       ,OrderCustomer.FixedNumber       ,
             gcExpectedFixedNumber).
checkChar(   "OrderCustomer.FirstName"         ,OrderCustomer.FirstName         ,
             gcExpectedFName).
checkChar(   "OrderCustomer.Language"          ,OrderCustomer.Language          ,
             gcExpectedLanguage).
checkChar(   "OrderCustomer.Surname1"          ,OrderCustomer.Surname1          ,
             gcExpectedLName).
checkChar(   "OrderCustomer.Surname2"          ,OrderCustomer.Surname2          ,
             gcExpectedLName2).
checkChar(   "OrderCustomer.Nationality"       ,OrderCustomer.Nationality       ,
             gcExpectedNationality).
checkChar(   "OrderCustomer.CustID"            ,OrderCustomer.CustID            ,
             gcExpectedPersonId).
checkChar(   "OrderCustomer.PostOffice"        ,OrderCustomer.PostOffice        ,
             gcExpectedPostOffice).
checkChar(   "OrderCustomer.region"            ,OrderCustomer.region            ,
             gcExpectedRegion).
checkChar(   "OrderCustomer.Language"          ,OrderCustomer.Language          ,
             gcExpectedSecOrdCustLanguage).
checkChar(   "OrderCustomer.Nationality"       ,OrderCustomer.Nationality       ,
             gcExpectedSecOrdCustNational).
checkChar(   "OrderCustomer.Country"           ,OrderCustomer.Country           ,
             gcExpectedSecOrdCustrCountry).
checkChar(   "OrderCustomer.MobileNumber"      ,OrderCustomer.MobileNumber      ,
             gcExpectedSMSNumber).
checkChar(   "OrderCustomer.SurName"           ,OrderCustomer.SurName           ,
             gcExpectedSurName).
checkChar(   "OrderCustomer.custtitle"         ,OrderCustomer.custtitle         ,
             gcExpectedTitle).
checkChar(   "OrderCustomer.ZIP"               ,OrderCustomer.ZIP               ,
             gcExpectedZipCode).
checkDate(   "OrderCustomer.BirthDay"          ,OrderCustomer.BirthDay          ,
             gdaExpectedBirthDay).
checkInt(    "OrderCustomer.RowType"           ,OrderCustomer.RowType           ,
             giExpectedRowType).
checkLogical("OrderCustomer.OperEmailMarketing",OrderCustomer.OperEmailMarketing,
             glExpectedAddressMarkEmail,'glExpectedAddressMarkEmail did not match.').
checkLogical("OrderCustomer.OutEmailMarketing" ,OrderCustomer.OutEmailMarketing ,
             glExpectedAddressMarkEmail3rd,'glExpectedAddressMarkEmail3rd did not match.').
checkLogical("OrderCustomer.OperPostMarketing" ,OrderCustomer.OperPostMarketing ,
             glExpectedAddressMarkPost,'glExpectedAddressMarkPost did not match.').
checkLogical("OrderCustomer.OutPostMarketing"  ,OrderCustomer.OutPostMarketing  ,
             glExpectedAddressMarkPost3rd,'glExpectedAddressMarkPost3rd did not match.').
checkLogical("OrderCustomer.OperSMSMarketing"  ,OrderCustomer.OperSMSMarketing  ,
             glExpectedAddressMarkSMS,'glExpectedAddressMarkSMS did not match.').
checkLogical("OrderCustomer.OutSMSMarketing"   ,OrderCustomer.OutSMSMarketing   ,
             glExpectedAddressMarkSMS3rd,'glExpectedAddressMarkSMS3rd did not match.').
checkLogical("OrderCustomer.OperAllMarketing"  ,OrderCustomer.OperAllMarketing  ,
             glExpectedOperAllMarketing,'glExpectedOperAllMarketing did not match.').


Table: OrderPayment


checkChar(   "OrderPayment.Brand"      ,OrderPayment.Brand      ,
             gcExpectedBrand).
checkChar(   "OrderPayment.CCReference",OrderPayment.CCReference,
             gcExpectedCCReference).
checkInt(    "OrderPayment.Method"     ,OrderPayment.Method     ,
             giExpectedMethod).


Table: OrderService


checkChar(   "OrderService.Brand"  ,OrderService.Brand  ,gcExpectedBrand).
checkInt(    "OrderService.OrderID",OrderService.OrderID,giExpectedOrderId).


Table: OrderTopup


checkDecimal("OrderTopup.Amount" ,OrderTopup.Amount ,gdeExpectedTopup).
checkInt(    "OrderTopup.OrderID",OrderTopup.OrderID,giExpectedOrderId).


Table: SIM


checkInt(    "SIM.simstat",SIM.simstat,giExpectedSIMStatus).



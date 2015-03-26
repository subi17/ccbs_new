DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColon AS INTEGER NO-UNDO.
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAccount =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddress =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressCity =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressCodC =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressCodP =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressConame =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressContactName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressCountry =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressCustomerName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressEmail =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressFName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressLName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressLName2 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressNationality =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressPhoneNumber =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressRegion =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressSiteName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressSmsNumber =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressStreet =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressTitle =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedAddressZip =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedBrand =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCampaign =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCCReference =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedChannel =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCLI =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCLIType =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCodC =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCodP =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCompany =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCompanyId =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCoName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedContractID =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCountry =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCreUser =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCreUser2 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCurrOper =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCustID =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedCustIDType =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedDeviceId =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedDeviceModel =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedEmail =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedFeeModel =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedFixedNumber =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedFName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedHostTable =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedICC =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedIdType =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedIMEI =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedKeyValue =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedLanguage =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedLName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedLName2 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMemoSeq =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMemoText =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMemoText2 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMemoText3 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMemoTitle =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMemoTitle2 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMemoTitle3 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedMsisdnStatusCode =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedNationality =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedNumberType =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedOldBillCategory =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedOldICC =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedOrderer =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedOrdererID =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedOrdererIdType =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedOrdererIp =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedOrderID =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedParamChannel =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedParamDeviceModel =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedParamIdType =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedParamIMEI =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedParamLanguage =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedPaymentMethod =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedPaymentReference =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedPersonId =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedPostOffice =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedRegion =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSalesman =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSecOrdCustLanguage =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSecOrdCustNational =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSecOrdCustrCountry =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedServices =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSMSNumber =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSource =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedStatusCode =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSubscriptionType =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedSurName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedTestDescription =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedTitle =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedUserFName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedUserLName =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedUserLName2 =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gcExpectedZipCode =  SUBSTR(cLine, iColon + 1).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdaExpectedAddressBirthDay = DATE( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdaExpectedBirthDay = DATE( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdeExpectedCreStamp = DECIMAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdeExpectedDevicePrice = DECIMAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdeExpectedDiscount = DECIMAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdeExpectedFAT = DECIMAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdeExpectedParamDevicePrice = DECIMAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). gdeExpectedTopup = DECIMAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedAddressLanguage = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedInvCustrole = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedMemoSeq = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedMethod = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedMnpStatus = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedMsSeq = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedOrderId = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedRowType = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedSIMStatus = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedStatusCode = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). giExpectedUserRole = INTEGER( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedAddressMarkEmail = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedAddressMarkEmail3rd = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedAddressMarkPost = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedAddressMarkPost3rd = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedAddressMarkSMS = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedAddressMarkSMS3rd = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedCheck = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedMarkEmail = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedMarkEmail3rd = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedMarkPost = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedMarkPost3rd = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedMarkSMS = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedMarkSMS3rd = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedMnpProcessBypass = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedNetworkBypass = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedNoCharge = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedOldPayType = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedOperAllMarketing = LOGICAL( SUBSTR(cLine, iColon + 1)).
IMPORT STREAM sData UNFORMATTED cLine. iColon = INDEX(cLine, ':'). glExpectedPayType = LOGICAL( SUBSTR(cLine, iColon + 1)).
/* Count of lines : 134 */

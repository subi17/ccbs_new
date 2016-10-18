CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TopUpGiftWaitDays"
   TMSParam.ParamGroup = "Logistics" 
   TMSParam.ParamType = "I"
   TMSParam.ParamName = "How many days of wait till the welcome gift topup"
   TMSParam.Online = FALSE
   TMSParam.IntVal = 14
   .

CREATE FeeModel.
ASSIGN
   FeeModel.FeeModel = "SHIPCOST"
   FeeModel.FeeName = "Shipping Cost"
   FeeModel.Brand = "1"
   .

CREATE FMItem.
ASSIGN
   FMItem.FeeModel = "SHIPCOST"
   FMItem.PriceList = "OrderCh"
   FMItem.BillCode = "SHIPPINGCOST"
   FMItem.BillMethod = YES
   FMItem.BillType = "DF"
   FMItem.FromDate = DATE(10,1,2016)
   FMItem.ToDate = DATE(12,31,2049)
   FMItem.Brand = "1"
   .

CREATE BillItem.
ASSIGN
   BillItem.BillCode = "SHIPPINGCOST"
   BillItem.BIName = "Shipping cost delivery fee"
   BillItem.AccNum = 75902000
   BillItem.BIGroup = "11"
   BillItem.EUAccNum = 75902000
   BillItem.FSAccNum = 75902000
   BillItem.Brand = "1"
   BillItem.EUConAccNum = 75902000
   BillItem.CostCentre = "SL"
   BillItem.AltAccNum = 75902000
   BillItem.TaxClass = "2"
   BillItem.VIPAccNum = 75902000
   .

CREATE BillItem.
ASSIGN
   BillItem.BillCode = "WELCOMEFAT"
   BillItem.BIName = "Welcome gift FAT"
   BillItem.AccNum = 70019000
   BillItem.BIGroup = "21"
   BillItem.EUAccNum = 70019000
   BillItem.FSAccNum = 70019000
   BillItem.Brand = "1"
   BillItem.EUConAccNum = 70019000
   BillItem.CostCentre = "SL"
   BillItem.AltAccNum = 70019000
   BillItem.TaxClass = "2"
   BillItem.VIPAccNum = 70019000
   .


CREATE FATGroup.
ASSIGN
   FATGroup.FtGrp = "WELCOMEGIFT"
   FATGroup.FtgName = "Welcome gift"
   FATGroup.BillCode = "WELCOMEFAT"
   FATGroup.Brand = "1"
   FATGroup.FATType = 2
   FATGroup.Transfer = YES
   FATGroup.FatTarget = "0"
   FATGroup.QtyUnit = "Amt"
   .
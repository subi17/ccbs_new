DEF VAR ldafrom AS DATE init 09/07/16.

FUNCTION create_feemodel RETURNS log (INPUT lcmodel AS CHAR,
                                      INPUT ligroup AS INT,
                                      INPUT lcname as char,
                                      INPUT ldamt AS DEC,
                                      INPUT lcBilltype AS CHAR,
                                      INPUT lcbrokenrental AS INT,
                                      INPUT lcPriceList AS CHAR,
                                      INPUT llMethod AS LOG,
                                      INPUT liInterval AS INT):

   FIND FIRST feemodel WHERE feemodel.brand EQ "1" AND
                             feemodel.feemodel EQ lcmodel NO-ERROR.
   IF NOT AVAIL Feemodel THEN DO:
      Create FeeModel.
      ASSIGN FeeModel.brand = "1" 
             Feemodel.feemodel = lcmodel
             feemodel.feename = lcname
             feemodel.fmgroup = ligroup.
   END.

   FIND FIRST fmitem WHERE fmitem.brand EQ "1" AND
                           fmitem.feemodel EQ lcmodel AND
                           fmitem.pricelist EQ "CONTRATOFIXED" NO-ERROR.
   IF NOT AVAIL fmitem THEN DO:
      CREATE fmitem.
      ASSIGN
         fmitem.amount = ldamt
         fmitem.billcode = lcmodel
         fmitem.billcycle = 1
         fmitem.billmethod = llMethod
         fmitem.billtype = lcBillType
         fmitem.brand = "1"
         fmitem.brokenrental = lcbrokenrental
         fmitem.feemodel = lcmodel 
         fmitem.todate = 12/31/49
         fmitem.firstmonthbr = 0
         fmitem.fromdate = ldafrom
         fmitem.pricelist = lcPriceList
         fmitem.interval = liInterval.

   END.
END.

create_feemodel("CONTDSL45MF",46,"Convergent ADSL monthly fee",37.19,"MF",1,"CONTRATOFIXED",FALSE,1).
create_feemodel("CONTDSL55MF",46,"Convergent ADSL monthly fee",45.45,"MF",1,"CONTRATOFIXED",FALSE,1).
create_feemodel("CONTFH45_50MF",46,"Convergent FIBER 50MB monthly fee",37.19,"MF",1,"CONTRATOFIXED",FALSE,1).
create_feemodel("CONTFH55_50MF",46,"Convergent FIBER 50MB monthly fee",45.45,"MF",1,"CONTRATOFIXED",FALSE,1).
create_feemodel("CONTFH55_300MF",46,"Convergent FIBER 300MB monthly fee",45.45,"MF",1,"CONTRATOFIXED",FALSE,1).
create_feemodel("CONTFH65_300MF",46,"Convergent FIBER 300MB monthly fee",53.72,"MF",1,"CONTRATOFIXED",FALSE,1).

create_feemodel("CONTS2GBMF",46,"Convergent mobile monthly fee",0.00,"MF",1,"CONTRATOFIXED",FALSE,1).
create_feemodel("CONTS10GBMF",46,"Convergent mobile monthly fee",0.00,"MF",1,"CONTRATOFIXED",FALSE,1).

create_feemodel("FTERMPERIOD",0,"Fixed line contract termination",100.0,"PF",1,"COMMON",TRUE,0).




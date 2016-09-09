DEF VAR ldafrom AS DATE init 09/07/16.

FUNCTION create_feemodel RETURNS log (INPUT lcmodel AS CHAR,
                                      INPUT ligroup AS INT,
                                      INPUT lcname as char,
                                      INPUT ldamt AS DEC):

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
         fmitem.billmethod = FALSE
         fmitem.billtype = "MF"
         fmitem.brand = "1"
         fmitem.brokenrental = 1
         fmitem.feemodel = lcmodel 
         fmitem.todate = 12/31/49
         fmitem.firstmonthbr = 0
         fmitem.fromdate = ldafrom
         fmitem.pricelist = "CONTRATOFIXED"
         fmitem.interval = 1.

   END.
END.

create_feemodel("CONTDSL45MF",46,"Contrato DSL45 Monthly fee",37.19).
create_feemodel("CONTDSL55MF",46,"Contrato DSL55 Monthly fee",45.45).
create_feemodel("CONTFH45_50MF",46,"Contrato FH45_50 Monthly fee",37.19).
create_feemodel("CONTFH55_50MF",46,"Contrato FH55_50 Monthly fee",45.45).
create_feemodel("CONTFH55_300MF",46,"Contrato FH55_300 Monthly fee",45.45).
create_feemodel("CONTFH65_300MF",46,"Contrato FH65_300 Monthly fee",53.72).

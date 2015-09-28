/* YPR-2173
Use this script to make Intelligent Landing Page configurations to DB
Execute this twice. 
The 1st run for DATA200_UPSELL and the another to DSS200_UPSELL.
Add/Remove comments in 'assign' for the executions.
*/
def var lcCliTypes as char no-undo.
def var liCount    as int  no-undo.
def var c as char no-undo.

lcCliTypes = "CONT,CONT15,CONT24,CONT4,CONT5,CONT6,CONT7,CONT8,CONT9,CONTD,CONTF,CONTFF,CONTM,CONTM2,CONTRD,CONTS,CONTSF".

do liCount = 1 to num-entries(lcCliTypes):

   create SLGAnalyse.
   assign
      SLGAnalyse.Brand = "1"
      SLGAnalyse.BelongTo = TRUE
      SLGAnalyse.Clitype = ENTRY(liCount,lcCliTypes,",")
      SLGAnalyse.BillCode = "14100001"
      SLGAnalyse.CCN = 93
      SLGAnalyse.ValidFrom = 06/04/2015
      SLGAnalyse.ValidTo = 12/31/2049
      SLGAnalyse.BDest = "GPRS"
      SLGAnalyse.SLGAType = 6
      /*For DATA200_UPSELL*/
      SLGAnalyse.ServiceLimitGroup = "DATA200_UPSELL"
      SLGAnalyse.Prior = 40.
      /*For DSS200_UPSELL*/
      /*
      SLGAnalyse.ServiceLimitGroup = "DSS200_UPSELL"
      SLGAnalyse.Prior = 07.
      */
end.

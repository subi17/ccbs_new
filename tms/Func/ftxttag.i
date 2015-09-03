/* ftxttag.i        16.03.04/aam
                    14.04.04/aam&jp subser and killms -tags
                    28.04.04/aam date+        
                    18.05.04/aam custnum, custaddr etc.
                    29.10.04/aam saldoagreement
                    27.01.06/aam lastname, firstname
                    21.02.06/aam ServiceList
                    25.05.07/aam contact nbrs
*/

DEF VAR lcTagOwner      AS CHAR NO-UNDO.
DEF VAR lcTagUser       AS CHAR NO-UNDO. 
DEF VAR lcTagPhone      AS CHAR NO-UNDO.
DEF VAR lcTagCLI        AS CHAR NO-UNDO. 
DEF VAR lcTagCLIType    AS CHAR NO-UNDO. 
DEF VAR lcTagRefNum     AS CHAR NO-UNDO.
DEF VAR lcTagGiven      AS CHAR NO-UNDO. 
DEF VAR lcTagInvAmt     AS CHAR NO-UNDO. 
DEF VAR liTagCustNum    AS INT  NO-UNDO.
DEF VAR lcTagSaldoAmt   AS CHAR NO-UNDO. 
DEF VAR lcTagSaldoDate  AS CHAR NO-UNDO.
DEF VAR lcTagLastName   AS CHAR NO-UNDO.
DEF VAR lcTagFirstName  AS CHAR NO-UNDO. 
DEF VAR lcTagCTName     AS CHAR NO-UNDO.
DEF VAR lcTagCustIDType AS CHAR NO-UNDO.
DEF VAR lcTagCustID     AS CHAR NO-UNDO.
DEF VAR lcTagDelAddress AS CHAR NO-UNDO.
DEF VAR lcTagDelPost    AS CHAR NO-UNDO.
DEF VAR lcTagUPSHours   AS CHAR NO-UNDO.
DEF VAR lcTagBankAcc    AS CHAR NO-UNDO.
DEF VAR lcTagContact1   AS CHAR NO-UNDO.
DEF VAR lcTagContact2   AS CHAR NO-UNDO.
DEF VAR lcTagMGMTienda  AS CHAR NO-UNDO. 
DEF VAR lcTagMGMReferee AS CHAR NO-UNDO.

/* corporate customer order confirmation email */
DEF VAR lcTagCompanyCIF    AS CHAR NO-UNDO.
DEF VAR lcTagCompanyFoundDate AS CHAR NO-UNDO.
DEF VAR lcTagCompanyName   AS CHAR NO-UNDO.
DEF VAR lcTagContactData   AS CHAR NO-UNDO.
DEF VAR lcTagCustEmail     AS CHAR NO-UNDO.
DEF VAR lcTagCustRegion    AS CHAR NO-UNDO.
DEF VAR lcTagLastName2     AS CHAR NO-UNDO.

FUNCTION fReplaceTag RETURNS CHAR
   (icText  AS CHAR,
    iiMSSeq AS INT).

   DEF VAR liTagPoint AS INT  NO-UNDO.
   DEF VAR lcTagChr   AS CHAR NO-UNDO.

   /* nothing to do */
   IF INDEX(icText,"#") = 0 THEN RETURN icText.

   /* replace tags with data */
   ASSIGN icText   = REPLACE(icText,"#REFNUM",lcTagRefNum)
          icText   = REPLACE(icText,"#INVAMT",lcTagInvAmt)
          icText   = REPLACE(icText,"#PHONE",lcTagPhone)
          icText   = REPLACE(icText,"#DATE",STRING(TODAY,"99.99.9999"))
          icText   = REPLACE(icText,"#PVM",STRING(TODAY,"99.99.9999"))
          icText   = REPLACE(icText,"#YEAR",STRING(YEAR(TODAY),"9999"))
          icText   = REPLACE(icText,"#CUSTNAME",lcEPLRName)
          icText   = REPLACE(icText,"#CUSTADDR",lcEPLRAddr)
          icText   = REPLACE(icText,"#CUSTPOST",lcEPLRPost)
          icText   = REPLACE(icText,"#CUSTNUM",STRING(liTagCustNum))
          icText   = REPLACE(icText,"#OWNER",lcTagOwner)
          icText   = REPLACE(icText,"#CLITYPE",lcTagCLIType)
          icText   = REPLACE(icText,"#CLI",lcTagCLI)
          icText   = REPLACE(icText,"#MSISDN",lcTagCLI)
          icText   = REPLACE(icText,"#USER",lcTagUser)
          icText   = REPLACE(icText,"#LUOVUTETTU",lcTagGiven) 
          icText   = REPLACE(icText,"#SALDOAGRBEG",lcTagSaldoDate)
          icText   = REPLACE(icText,"#SALDOAGR",lcTagSaldoAmt)
          icText   = REPLACE(icText,"#LASTNAME2",lcTagLastName2)
          icText   = REPLACE(icText,"#LASTNAME",lcTagLastName)
          icText   = REPLACE(icText,"#FIRSTNAME",lcTagFirstName)
          icText   = REPLACE(icText,"#DELADDR",lcTagDelAddress)
          icText   = REPLACE(icText,"#DELPOST",lcTagDelPost)
          icText   = REPLACE(icText,"#UPSHOURS",lcTagUPSHours)
          icText   = REPLACE(icText,"#MGMTIENDA",lcTagMGMTienda)
          icText   = REPLACE(icText,"#MGMREFEREE",lcTagMGMReferee)
          icText   = REPLACE(icText,"#CUSTBANK",lcTagBankAcc)
          icText   = REPLACE(icText,"#CIDTYPE",lcTagCustIDType)
          icText   = REPLACE(icText,"#CUSTID",lcTagCustID)
          icText   = REPLACE(icText,"#CONTACTNBR1",lcTagContact1)
          icText   = REPLACE(icText,"#CONTACTNBR2",lcTagContact2)

          icText   = REPLACE(icText,"#CUSTREGION",lcTagCustRegion)
          icText   = REPLACE(icText,"#EMAIL",lcTagCustEmail)
          icText   = REPLACE(icText,"#CONDATA",lcTagContactdata)
          icText   = REPLACE(icText,"#COMPANYCIF",lcTagCompanyCIF)
          icText   = REPLACE(icText,"#COMPANYFOUNDDATE",lcTagCompanyFoundDate)
          icText   = REPLACE(icText,"#COMPANYNAME",lcTagCompanyName)
          icText   = REPLACE(icText,"#CONTACTDATA",lcTagContactData).

   /* termination */ 
   IF INDEX(icText,"#TERMDATE") > 0 THEN DO:
      FIND KillMS WHERE 
           KillMS.MSSeq = iiMSSeq NO-LOCK NO-ERROR.
             
      IF AVAIL KillMS THEN 
         icText = REPLACE(icText,"#TERMDATE",
                                 STRING(KillMS.KillDate,"99-99-9999")).
   END.

   IF INDEX(icText,"#FAX") > 0 THEN DO:
      FIND First SubSer where 
                 SubSer.MSSeq = iiMSSeq  and 
                 SubSer.servcom = "T62" NO-LOCK NO-ERROR.
      IF AVAIL SubSer THEN 
         icText = REPLACE(icText,"#FAX",SubSer.ssparam).
   END.   

   IF INDEX(icText,"#DATA") > 0 THEN DO:
      FIND First SubSer where 
                 SubSer.MSSeq   = iiMSSeq  and 
                 SubSer.servcom = "B16" NO-LOCK NO-ERROR.
      IF AVAIL SubSer THEN icText = REPLACE(icText,"#DATA",SubSer.ssparam).
      ELSE ictext = REPLACE(icText,"#DATA","tuntematon").
   END.

   RETURN icText.
   
END FUNCTION.

&IF "{&ServiceListTag}" = "YES"
&THEN
FUNCTION fMsServiceList RETURNS CHARACTER
   (icText     AS CHAR,
    iiMsSeq    AS INT,
    iiLanguage AS INT).

   DEF VAR lcServiceList  AS CHAR NO-UNDO.
   DEF VAR liServiceOn    AS INT  NO-UNDO.
   DEF VAR liServiceValue AS INT  NO-UNDO.
   DEF VAR lcServicePara  AS CHAR NO-UNDO.
   DEF VAR ldCurrent      AS DEC  NO-UNDO.
   DEF VAR liSerCnt       AS INT  NO-UNDO.
   DEF VAR lcBarrList     AS CHAR NO-UNDO.
   
   DEF BUFFER bSerMob FOR MobSub.
   

   IF INDEX(icText,"#SERVICELIST") = 0 THEN RETURN icText.
   
   /* get active (additional) services */
    
   /* voicemail */
   IF fCurrSubSer(iiMsSeq,"PP2") > 0 THEN ASSIGN 
      lcServicePara = fCurrSubSerPara(iiMsSeq,"PP2","MSISDN2")
      lcServiceList = fTeksti(220,iiLanguage) + " " + lcServicePara + CHR(10).

   /* saldo service */
   liServiceOn = fCreditTypeValue(iiMsSeq,
                                  OUTPUT liServiceValue).
   /* saldo agreement */
   IF liServiceOn = 3 THEN 
      lcServiceList = lcServiceList + 
                      fTeksti(221,iiLanguage) + " " +
                      STRING(liServiceValue) + " eur" + CHR(10).
   /* saldo reminder */
   ELSE IF liServiceOn = 2 THEN 
      lcServiceList = lcServiceList + 
                      fTeksti(222,iiLanguage) + " " +
                      STRING(liServiceValue) + " eur" + CHR(10).
      
   /* call specification */
   IF STRING(fCallSpecReport(iiMsSeq)) > "" THEN 
      lcServiceList = lcServiceList + 
                      fTeksti(223,iiLanguage) + CHR(10).

   ldCurrent = YEAR(TODAY) * 10000 +
               MONTH(TODAY) * 100  +
               DAY(TODAY) + (TIME / 100000). 
               
   /* packages */
   FOR EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.MsSeq   = iiMsSeq   AND 
            MServiceLimit.FromTS  < ldCurrent AND
            MServiceLimit.EndTS   > ldCurrent,
      FIRST ServiceLimit NO-LOCK WHERE
            ServiceLimit.SLSeq = MServiceLimit.SLSeq,
      FIRST ServiceLimitGroup NO-LOCK WHERE
            ServiceLimitGroup.Brand     = gcBrand AND
            ServiceLimitGroup.GroupCode = ServiceLimit.GroupCode:
            
      lcServiceList = lcServiceList + 
                      ServiceLimitGroup.GroupName + CHR(10).
   END. 

   /* secret nbr */
   IF fSecretValue(iiMsSeq) THEN
      lcServiceList = lcServiceList + 
                      fTeksti(224,iiLanguage) + CHR(10).
                      
   /* PNP */
   FOR FIRST bSerMob NO-LOCK WHERE
             bSerMob.MsSeq = iiMsSeq,
        EACH PNPGroup NO-LOCK WHERE
             PNPGroup.Brand     = gcBrand     AND
             PNPGroup.GroupType = 2           AND
             PNPGroup.PNPGroup  = bSerMob.CLI AND
             PNPGroup.DTo      >= TODAY       AND
             PNPGroup.DFrom    <= TODAY:
   
      ASSIGN lcServicePara = fTeksti(225,iiLanguage)
             liSerCnt      = LENGTH(lcServicePara).
             
      FOR EACH PNPList NO-LOCK WHERE
               PNPList.Brand     = gcBrand         AND
               PNPList.PNPSeq    = PNPGroup.PNPSeq AND
               PNPList.ToDate   >= TODAY           AND
               PNPList.FromDate <= TODAY:
               
         IF lcServicePara = "" 
         THEN lcServiceList = lcServiceList + FILL(" ",liSerCnt).
         ELSE lcServiceList = lcServiceList + lcServicePara.
           
         ASSIGN 
            lcServiceList = lcServiceList + " " + PNPList.CLI + CHR(10)
            lcServicePara = "".
      END.         
   END.

   /* barrings */
   lcBarrList = "HES,PES,VEA,VES,SMA,SMH,SMP,SMV".
   
   DO liSerCnt = 1 TO NUM-ENTRIES(lcBarrList):
   
      liServiceValue = fCurrSubSer(iiMsSeq,
                                   ENTRY(liSerCnt,lcBarrList)).
                                   
      IF liServiceValue > 0 THEN DO:
         FIND ServCom WHERE 
              ServCom.Brand   = gcBrand AND
              ServCom.ServCom = ENTRY(liSerCnt,lcBarrList) NO-LOCK NO-ERROR.
         IF AVAILABLE ServCom 
         THEN lcServicePara = ServCom.SCLocalName.
         ELSE lcServicePara = ENTRY(liSerCnt,lcBarrList).
    
         lcServiceList = lcServiceList + lcServicePara + CHR(10).
      END.
   END.
   
   icText = REPLACE(icText,"#SERVICELIST",lcServiceList).
    
   RETURN icText.
   
END FUNCTION.
&ENDIF



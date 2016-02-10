/* ----------------------------------------------------------------------
  MODULE .......: FUNC.I
  TASK .. ......: Common functions
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 30.07.2002
  MODIFIED .....: 24.10.02/aam fTMSCodeName
                  10.06.03/aam fTmsCodeList, fTMSCodeChk
                  09.09.03/aam brand 
                  10.11.03/ jp new output parameter for fpnpcheck
                  23.04.04/aam InvType 4 for fInvBal
                  06.05.04/aam check numbers with "index" in fHideBSub
                  28.06.04/aam fHdrText
                  04.11.04/aam fInvBal removed
                  08.07.05/jp  global define for timestamp function
                  29.09.05/aam fTSDuration
                  13.12.05/aam fChkPersonID, fChkCompanyID, fDispCustName
                  18.01.06/aam fPrintCustName
                  20.11.06/aam fDispOrderName
                  01.12.06/aam firstname first in fdispcust and fdisporder
                  12.12.06/aam fWriteMemo

  VERSION ......: m15
  CONTENTS......: Time and date fuctions
                     - fDateFmt
                     - fAddCent
                     - fSec2C
                  Timestamp functions
                     - fMakeTS
                     - fSplitTS
                     - fTS2HMS
                     - fHMS2TS
                  Misc functions
                     - fTMSCodeName
                     - fTMSCodeList 
                     - fIsPNP
                     - fBDestName
                     - fPNPCheck
                     - fGetPNPGroup
                     - fHideBSub
                     - fReplaceSMS
                     - fHdrText

----------------------------------------------------------------------- */

&IF "{&BrandVarDefined}" NE "YES"
&THEN
DEF SHARED VAR gcBrand   like customer.brand NO-UNDO.
DEF SHARED VAR katun     AS CHAR.
&ENDIF



/* -------------------------
  Time and date functions
-------------------------- */
&IF "{&fdatefmt}" NE "YES"
&THEN
&GLOBAL-DEFINE fdatefmt YES




/* FUNCTION FOR converting Date FORMAT */
FUNCTION fDateFmt RETURNS CHAR
   (INPUT d AS Date, INPUT f AS CHAR):

   DEF VAR yy   AS i.
   DEF VAR mm   AS i.
   DEF VAR dd   AS i.
   DEF VAR cy   AS c.
   DEF VAR cm   AS c.
   DEF VAR cd   AS c.
   DEF VAR i    AS i.
   DEF VAR sep  AS c.
   DEF VAR amt  AS i EXTENT 5.
   DEF VAR pos  AS i EXTENT 3.
   DEF VAR spos AS i EXTENT 2.
   DEF VAR ret  AS c.
   DEF VAR b-ok AS lo.

   /* separate INTEGER values */
   ASSIGN
      yy = year(d)
      mm = month(d)
      dd = day(d).

   /* scan YEAR - MONTH - DAY FORMAT */
   DO i = 1 TO length(f):

      amt[5] = amt[5] + 1.

      case substr(f,i,1):

         when "y" THEN DO:
            amt[1] = amt[1] + 1.
            IF pos[1] = 0 THEN pos[1] = i.
         END.

         when "m" THEN DO:
            amt[2] = amt[2] + 1.
            IF pos[2] = 0 THEN pos[2] = i.
         END.

         when "d" THEN DO:
            amt[3] = amt[3] + 1.
            IF pos[3] = 0 THEN pos[3] = i.
         END.

         otherwise DO:
            amt[4] = amt[4] + 1.
            sep = substr(f,i,1).
            IF spos[1] = 0 THEN spos[1] = i.
            ELSE spos[2] = i.
         END.

      END.

   END.

   IF yy > 9 AND amt[1] = 1 THEN 
      ASSIGN amt[1] = 2 amt[5] = amt[5] + 1.
   IF mm > 9 AND amt[2] = 1 THEN 
      ASSIGN amt[2] = 2 amt[5] = amt[5] + 1.
   IF dd > 9 AND amt[3] = 1 THEN 
      ASSIGN amt[3] = 2 amt[5] = amt[5] + 1.

   /* check that incoming FORMAT is OK */
   b-ok = (amt[1] > 4 OR
           amt[2] > 2 OR
           amt[3] > 2 OR
           amt[4] > 2) = FALSE.

   IF b-ok THEN DO:

      /* INTEGER values into characters */
      ASSIGN
         cy = string(yy,"9999")
         cm = string(mm,"99")
         cd = string(dd,"99").

      /* FORMAT lengths */
      cy = substr(cy,length(cy) + 1 - amt[1]).
      IF mm < 10 THEN
         cm = substr(cm,length(cm) + 1 - amt[2]).
      ELSE amt[2] = 2.
      IF dd < 10 THEN
         cd = substr(cd,length(cd) + 1 - amt[3]).
      ELSE amt[3] = 2.

      /* build RETURN STRING in right order */
      DO i = 1 TO amt[5]:
         IF pos[1]  = i THEN ret = ret + cy.
         IF pos[2]  = i THEN ret = ret + cm.
         IF pos[3]  = i THEN ret = ret + cd.
         IF spos[1] = i THEN ret = ret + sep.
         IF spos[2] = i THEN ret = ret + sep.
      END.

   END.
   ELSE ret = ?.

   RETURN ret.             

END.

/* Add century TO a Date using -yy PARAMETER */
FUNCTION fAddCent RETURNS INTEGER
  (INPUT yyy AS INT).

   DEF VAR ret AS i NO-UNDO.

   ret = session:year-offset.

   case ret - yyy <= truncate(ret / 100,0) * 100.

      when FALSE THEN ret = (truncate(ret / 100,0) + 1) * 100.

      when TRUE  THEN ret = truncate(ret / 100,0) * 100.

   END.   

   RETURN ret + yyy.

END.   

&ENDIF
FUNCTION fSec2C RETURNS CHAR
  (INPUT tme AS INT, INPUT hformat AS CHAR).    

   DEF VAR tmp AS i NO-UNDO.
   DEF VAR hh  AS i NO-UNDO.
   DEF VAR mm  AS i NO-UNDO.
   DEF VAR ss  AS i NO-UNDO.

   ASSIGN
      tmp = tme
      hformat = "99" WHEN hformat = ""
      hh  = truncate(tmp / 3600,0)
      tmp = tmp MODULO 3600
      mm  = truncate(tmp / 60,0)
      ss  = tmp MODULO 60.

   return string(hh,hformat) + ":" +
          string(mm,"99")    + ":" +
          string(ss,"99").

END.

/* --------------------------
   Timestamp functions
-------------------------- */


&IF "{&fmakets}" NE "YES"
&THEN

&GLOBAL-DEFINE fmakets YES



function fMakeTS returns dec.

   def var yy  as i no-undo.
   def var mm  as i no-undo.
   def var dd  as i no-undo.
   def var ret as de no-undo format "99999999.99999".

   assign
      yy = year(today)
      mm = month(today)
      dd = day(today).

   ret = yy * 10000 + mm * 100 + dd.
   ret = ret + (time / 100000).

   return ret.

end.


function fSplitTS returns log
  (input ts as dec, output dte as date, output tme as int).

   def var yy  as i  no-undo.
   def var mm  as i  no-undo.
   def var dd  as i  no-undo.
   def var i   as i  no-undo.
   def var c   as c  no-undo.
   def var ret as lo no-undo.

   assign
      c   = substr(string(ts,"99999999.99999"),1,8)
      yy  = integer(substr(c,1,4))
      mm  = integer(substr(c,5,2))
      dd  = integer(substr(c,7,2))
      dte = date(mm,dd,yy)
      c   = substr(string(ts,"99999999.99999"),10,5)
      tme = integer(c)
   no-error.

   if error-status:error then ret = false.
   else ret = true.

   return ret.

end.

FUNCTION fTS2HMS RETURNS CHARACTER
  (INPUT tstamp AS DECIMAL).

   DEF VAR outstring  AS C  NO-UNDO.
   DEF VAR dte        AS DA NO-UNDO.
   DEF VAR tme        AS I  NO-UNDO.

   IF fSplitTS(tstamp, OUTPUT dte, OUTPUT tme) THEN.

   outstring = STRING(dte,"99.99.9999") + " " + STRING(tme,"hh:mm:ss").
   RETURN outstring.

END FUNCTION.

FUNCTION fHMS2TS RETURNS DECIMAL
  (INPUT pDate AS DATE, INPUT pTime AS CHARACTER).

   DEF VAR lRet AS DECIMAL.
   DEF VAR lSec AS INTEGER.

   assign
      lSec = INT(ENTRY(1,pTime,":")) * 3600 +
             INT(ENTRY(2,pTime,":")) * 60   +
             INT(ENTRY(3,pTime,":")) WHEN SUBSTR(pTime,3,1) = ":"
      lSec = INT(SUBSTR(pTime,1,2))  * 3600 +
             INT(SUBSTR(pTime,3,2))  * 60   +
             INT(SUBSTR(pTime,5,2))  WHEN SUBSTR(pTime,3,1) NE ":"
      lRet = YEAR(pDate) * 10000 + MONTH(pDate) * 100 + DAY(pDate) +
             lSec / 100000.

   RETURN lRet.

END FUNCTION.  
&ENDIF

/* -----------------
   Misc functions
------------------ */   

/* get the description for status codes etc. */
FUNCTION fTMSCodeName RETURNS CHARACTER
   (iTableName AS CHAR,
    iFieldName AS CHAR,
    iCodeValue AS CHAR).

   FIND FIRST TMSCodes NO-LOCK WHERE
              TMSCodes.TableName = iTableName AND
              TMSCodes.FieldName = iFieldName AND
              TMSCodes.CodeValue = iCodeValue NO-ERROR.

   IF AVAILABLE TMSCodes AND TMSCodes.InUse > 0 THEN RETURN TMSCodes.CodeName.
   ELSE RETURN "".

END FUNCTION.

/* check that used code is valid */
FUNCTION fTMSCodeChk RETURNS LOGICAL 
   (iTableName AS CHAR,
    iFieldName AS CHAR,
    iCodeValue AS CHAR).

   RETURN CAN-FIND(FIRST TMSCodes WHERE
                   TMSCodes.TableName = iTableName AND
                   TMSCodes.FieldName = iFieldName AND
                   TMSCodes.CodeValue = iCodeValue AND
                   TMSCodes.InUse > 0).

END FUNCTION.

/* get a list of all codes */
FUNCTION fTMSCodeList RETURNS CHARACTER
   (iTableName AS CHAR,
    iFieldName AS CHAR).

   DEF VAR lcCodeLst AS CHAR NO-UNDO.

   lcCodeLst = "".

   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = iTableName AND
            TMSCodes.FieldName = iFieldName AND
            TMSCodes.InUse > 0:

      lcCodeLst = lcCodeLst          + 
                  (IF lcCodeLst > ""
                   THEN CHR(1)
                   ELSE "")          + 
                  TMSCodes.CodeValue + 
                  CHR(9)             +
                  TMSCodes.CodeName. 
   END.                  

   RETURN lcCodeLst.

END FUNCTION.


/* Is this  a PNP number */
FUNCTION fIsPNP RETURNS LOGICAL
  (INPUT iCustNum AS INT,
   INPUT iBSub    AS CHAR,
   INPUT iPrefix  AS CHAR).

   DEF VAR lcPNPPref AS CHAR NO-UNDO INIT "MOB".
   RETURN 
      LOOKUP(iPrefix,lcPNPPref) > 0 AND
      CAN-FIND(FIRST PNPList WHERE
                     PNPList.Brand      = gcBrand  AND
                     PNPList.CustNum    = iCustNum AND
                     PNPList.BDestFrom <= iBSub    AND
                     PNPList.BDestTo   >= iBSub).

END.   

/* get B Destination name, either from PNP or BDest */
FUNCTION fBDestName RETURNS CHARACTER
  (iCustNum AS INT,
   iBSub    AS CHAR,
   iBDest   AS CHAR,
   iPrefix  AS CHAR):

   DEF VAR lServName AS CHAR NO-UNDO.

   IF fIsPNP(iCustNum,iBSub,iPrefix) THEN DO:

      FIND FIRST PNPList WHERE
                 PNPList.Brand      = gcBrand  AND
                 PNPList.CustNum    = iCustNum AND
                 PNPList.BDestFrom >= iBSub    AND
                 PNPList.BDestTO   <= iBSub
      NO-LOCK NO-ERROR.

      IF AVAIL PNPList THEN DO:
         FIND FIRST PNPGroup OF PNPList NO-LOCK NO-ERROR.
         lServName = PNPGroup.Name.
      END.

   END.

   IF lServName = "" THEN DO:
      FIND FIRST BDest where
                 BDest.Brand = gcBrand AND
                 BDest.BDest = iBDest 
      NO-LOCK NO-ERROR.
      IF AVAIL BDest THEN lServName = BDest.BDName.
   END.

   RETURN lServName.

END.

/* check if dialled number is a PNP number */
FUNCTION fPNPCheck RETURNS LOGICAL
  (INPUT  iCustNum AS INT,
   INPUT  iBSub    AS CHAR,
   INPUT  iPrefix  AS CHAR,
   OUTPUT pCCN     AS INT,
   OUTPUT pRateCCN AS INT,
   OUTPUT pPLCode  AS CHAR):

   ASSIGN
      pCCN    = 0
      pPLCode = "".

   IF fIsPNP(iCustNum,iBSub,iPrefix) THEN DO:

      FIND FIRST PNPList WHERE
                 PNPList.CustNum    = iCustNum AND
                 PNPList.BDestFrom <= iBSub    AND
                 PNPList.BDestTo   >= iBSub
      NO-LOCK NO-ERROR.

      FIND FIRST PNPGroup OF PNPList
      NO-LOCK NO-ERROR.

      IF AVAIL PNPGroup THEN ASSIGN
         pCCN     = PNPGroup.CCN 
         pRateCCN = PNPGroup.RateCCN
         pPLCode  = PNPList.PriceList.

   END.

   RETURN (pCCN NE 0).

END FUNCTION.

/* Get PNPGroup for B Destination */
FUNCTION fGetPNPGroup RETURNS CHARACTER
  (INPUT icBDest AS CHAR):

   DEF VAR liLoop AS INT  NO-UNDO.
   DEF VAR lcRet  AS CHAR NO-UNDO.

   DO liLoop = 8 TO 1 BY -1:

      IF CAN-FIND(FIRST BDest WHERE
                        BDest.Brand = gcBrand AND
                        BDest.BDest = SUBSTR(icBDest,1,liLoop)) THEN DO: 

         FIND FIRST RateCCN WHERE
                    RateCCN.Brand = gcBrand AND
                    RateCCN.BDest = SUBSTR(icBDest,1,liLoop)
         NO-LOCK NO-ERROR.

         IF AVAIL RateCCN THEN DO:

            FIND FIRST PNPGroup WHERE
                       PNPGroup.Brand = gcBrand AND
                       PNPGroup.CCN   = RateCCN.CCN
            NO-LOCK NO-ERROR.

            IF AVAIL PNPGroup THEN lcRet = PNPGroup.PNPGroup.

         END.

         liLoop = 0.

      END.

   END.

   RETURN lcRet.

END FUNCTION.

FUNCTION fHideBSub RETURNS CHAR
  (INPUT str-in AS CHAR, 
   INPUT CustNo AS INT,
   INPUT bdest  AS CHAR,
   INPUT iiDestType AS INT,
   INPUT prefix AS CHAR,
   INPUT bHide  AS LOG).

   DEF VAR clean       AS lo NO-UNDO.
   DEF VAR loop        AS i  NO-UNDO.
   DEF VAR Qty         AS i  NO-UNDO.
   DEF VAR repl        AS c  NO-UNDO.
   DEF VAR showbnumber as c no-undo.

   /* move default values to cparam */
   qty = 3.
   repl = "X".
   showbnumber = "3,15".  

   clean = TRUE.

   IF iiDestType = 9 THEN DO:
      IF str-in BEGINS "0100" OR 
         str-in BEGINS "0101" OR 
         str-in BEGINS "0200" OR 
         str-in BEGINS "0202" OR 
         str-in BEGINS "0209" OR 
         str-in BEGINS "0300" OR 
         str-in BEGINS "060" OR 
         str-in BEGINS "0700" OR 
         str-in BEGINS "106" OR 
         str-in BEGINS "100"
      THEN bhide = FALSE.
      else bhide = TRUE.
   END.
   ELSE IF LOOKUP(STRING(iiDestType),showbnumber) > 0 THEN bhide = FALSE.   

   IF LENGTH(str-in) > Qty + 1 AND bHide THEN DO 
   loop = LENGTH(str-in) TO (LENGTH(str-in) - Qty + 1) BY -1.
      IF INDEX("0123456789",SUBSTRING(str-in,loop,1)) > 0 THEN
      SUBSTR(str-in,loop,1) = repl.
   END.

   RETURN str-in.
END.

FUNCTION fHdrText RETURNS CHARACTER
   (INPUT iiTxtNbr   AS INT, 
    INPUT iiLanguage AS INT).

   FIND FIRST HdrText NO-LOCK WHERE 
              HdrText.Brand  = gcBrand   AND
              HdrText.te-nro = iiTxtNbr  AND
              HdrText.te-kie = iiLanguage NO-ERROR.
   IF AVAIL HdrText THEN RETURN HdrText.te-text.

   /* if text on chosen language is not available use default */
   FIND FIRST HdrText NO-LOCK WHERE 
              HdrText.Brand  = gcBrand  AND
              HdrText.te-nro = iiTxtNbr AND  
              HdrText.te-kie = 1 NO-ERROR.
   IF AVAIL HdrText THEN RETURN HdrText.te-text.

   /* nothing was found */
   RETURN "".

END FUNCTION.


{Func/freplacesms.i}


FUNCTION fChkSaldoAccount RETURNS DEC
         (INPUT   CustNo       AS INT,
          INPUT   Cli          AS CHAR,
          INPUT   period       AS INT,
          INPUT   lcSaldofatime AS CHAR).


   DEF VAR ldsaldo AS DECIMAL  NO-UNDO.
   ldSaldo =  0.
   
   IF period = 0 THEN Period = YEAR(today) * 100 +     MONTH(today).
   
   FOR EACH FaTime WHERE
            FaTime.CustNum  = CustNo        AND
            FaTime.Cli      = Cli           AND
            FaTime.ftgrp    = lcSaldoFatime AND 
            Fatime.Invnum   = 0             AND
            Fatime.Period  <= Period NO-LOCK.
      
      IF      Fatime.Period = period THEN ldsaldo = ldsaldo + Fatime.Amt.
      ELSE IF Fatime.Period < period AND 
              Fatime.Transperiod > 0 THEN  ldsaldo = ldsaldo + Fatime.Amt.
   END.
   
   RETURN ldSaldo.

END.

&IF "{&fmake2dt}" NE "YES"
&THEN

&GLOBAL-DEFINE fmake2dt YES

function fMake2Dt returns dec (input tsdate as DATE , input tstime as INT).

   def var yy  as i no-undo.
   def var mm  as i no-undo.
   def var dd  as i no-undo.
   def var ret as de no-undo format "99999999.99999".

   assign
   yy = year(tsdate)
   mm = month(tsdate)
   dd = day(tsdate).

   ret = yy * 10000 + mm * 100 + dd.
   ret = ret + (tstime / 100000).

   return ret.

end.

&ENDIF
function fMakeOfficeTS returns DECIMAL.

   DEF VAR ldeCurrentTS AS DE NO-UNDO FORMAT "99999999.99999".
   DEF VAR ldeSeconds   AS DE NO-UNDO FORMAT "9.999999".
   DEF VAR liCompare    AS I  NO-UNDO.                  
   DEF VAR ldeNewStamp  AS DE NO-UNDO FORMAT "99999999.99999".  

   ASSIGN
      ldeCurrentTS = fmakets()
      ldeSeconds   = ldeCurrentTS - (INT(SUBSTRING(STRING(ldeCurrentTS),1,8)))
      liCompare    = ldeSeconds * 100000.

   IF    liCompare < 8  * 3600 THEN DO:
      ldeNewStamp = fmake2Dt(today, 8 * 3600).
   END.
   ELSE  IF liCompare > 21 * 3600 THEN DO:
      ldeNewStamp = fmake2Dt(today + 1, 8 * 3600).
   END.   
   ELSE DO:
      ldeNewstamp = ldeCurrentTS.
   END.

   RETURN ldeNewStamp.

END. 

/* duration between two timestamps */
FUNCTION fTSDuration RETURNS INTEGER
   (INPUT  idFromStamp AS DEC,
    INPUT  idToStamp   AS DEC,
    OUTPUT oiSeconds   AS INT).

   DEF VAR liDays  AS INT  NO-UNDO.
   DEF VAR ldtFrom AS DATE NO-UNDO.
   DEF VAR liFrom  AS INT  NO-UNDO.
   DEF VAR ldtTo   AS DATE NO-UNDO.
   DEF VAR liTo    AS INT  NO-UNDO.
   
   IF idToStamp <= idFromStamp THEN RETURN 0.
   
   fSplitTS(idFromStamp,
            OUTPUT ldtFrom,
            OUTPUT liFrom).
   fSplitTS(idToStamp,
            OUTPUT ldtTo,
            OUTPUT liTo).
            
   liDays = ldtTo - ldtFrom.
       
   IF liTo < liFrom THEN ASSIGN
      liDays    = liDays - 1
      oiSeconds = 86400 - liFrom + liTo.
   
   ELSE oiSeconds = liTo - liFrom.
   
   RETURN liDays.
    
END FUNCTION.

/* check finnish personid */
FUNCTION fChkPersonID RETURNS LOGICAL
   (INPUT icPersonID AS CHAR).

   DEF VAR liPISum     AS INT  NO-UNDO.
   DEF VAR lcModChar   AS CHAR NO-UNDO.
   DEF VAR lcModLetter AS CHAR NO-UNDO INIT
      "A,B,C,D,E,F,H,J,K,L,M,N,P,R,S,T,U,V,W,X,Y".
   
   IF LENGTH(icPersonID) < 11 THEN RETURN FALSE.
   
   IF LOOKUP(SUBSTRING(icPersonID,7,1),"+,-,A") = 0 THEN RETURN FALSE.

   liPISum = INTEGER(SUBSTRING(icPersonID,1,6) + 
                     SUBSTRING(icPersonID,8,3)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN FALSE. 

   liPISum = liPISum MOD 31.
   
   IF liPISum < 10 
   THEN lcModChar = STRING(liPISum).
   ELSE lcModChar = ENTRY(liPISum - 9,lcModLetter).
   
   RETURN (lcModChar = SUBSTRING(icPersonID,11,1)).
   
END FUNCTION.

/* check finnish companyid */
FUNCTION fChkCompanyID RETURNS LOGICAL
   (INPUT icCompanyID AS CHAR).

   DEF VAR liCICount   AS INT  NO-UNDO.
   DEF VAR liCISum     AS INT  NO-UNDO.

   IF LENGTH(icCompanyID) < 7 THEN RETURN FALSE.
   
   /* add leading zeros */
   IF LENGTH(icCompanyID) < 9 THEN DO:
      IF INDEX(icCompanyID,"-") > 0 
      THEN liCICount = 9.
      ELSE liCICount = 8.
      
      icCompanyID = FILL("0",liCICount - LENGTH(icCompanyID)) + icCompanyID.
   END.

   liCISum = 7  * INTEGER(SUBSTRING(icCompanyID,1,1)) +
             9  * INTEGER(SUBSTRING(icCompanyID,2,1)) +
             10 * INTEGER(SUBSTRING(icCompanyID,3,1)) +
             5  * INTEGER(SUBSTRING(icCompanyID,4,1)) +
             8  * INTEGER(SUBSTRING(icCompanyID,5,1)) +
             4  * INTEGER(SUBSTRING(icCompanyID,6,1)) +
             2  * INTEGER(SUBSTRING(icCompanyID,7,1)).

   liCICount = liCISum MOD 11.
   
   IF liCICount > 0 THEN liCICount = 11 - liCICount.
   
   RETURN (STRING(liCICount) = SUBSTRING(icCompanyID,LENGTH(icCompanyID),1)).
   
END FUNCTION.

/* customer's name to be displayed */
FUNCTION fDispCustName RETURNS CHARACTER
   (BUFFER ibNameCust FOR Customer). 
   
   IF NOT AVAILABLE ibNameCust THEN RETURN "". 

  /* company name may be divided into two rows */
   IF ibNameCust.CustIDType = "CIF" AND ibNameCust.CompanyName > "" THEN
      RETURN ibNameCust.CompanyName + 
             (IF ibNameCust.CoName > "" 
              THEN " " + ibNameCust.CoName
              ELSE "").
    
   /* private customers have both lastname and firstname */
   ELSE RETURN ibNameCust.FirstName + " " + ibNameCust.CustName + 
               (IF ibNameCust.SurName2 > "" 
                THEN " " + ibNameCust.SurName2
                ELSE "").
   
END FUNCTION.

/* customer's name to be printed */
FUNCTION fPrintCustName RETURNS CHARACTER
   (BUFFER ibNameCust FOR Customer). 
   
   IF NOT AVAILABLE ibNameCust THEN RETURN "".

   /* company name may be divided into two rows, but it is printing routine's
      job to use or not use COName */
   IF ibNameCust.CustIDType = "CIF" AND ibNameCust.CompanyName > "" THEN
      RETURN ibNameCust.CompanyName.
    
   /* private customers have both lastname and firstname */
   ELSE RETURN ibNameCust.FirstName + " " + ibNameCust.CustName + 
               (IF ibNameCust.SurName2 > "" 
                THEN " " + ibNameCust.SurName2
                ELSE "").
    
END FUNCTION.
    
FUNCTION fDispOrderName RETURNS CHARACTER
   (BUFFER ibNameOrder FOR OrderCustomer). 
   
   IF NOT AVAILABLE ibNameOrder THEN RETURN "".
   
   IF ibNameOrder.CustIDType = "CIF" AND ibNameOrder.Company > "" THEN
      RETURN ibNameOrder.Company.

   /* private customers have both lastname and firstname */
   ELSE RETURN ibNameOrder.FirstName + " " + ibNameOrder.SurName1 + 
               (IF ibNameOrder.SurName2 > "" 
                THEN " " + ibNameOrder.SurName2
                ELSE ""). 
   
END FUNCTION.

FUNCTION fPrintOrderName RETURNS CHARACTER
   (BUFFER ibNameOrder FOR OrderCustomer). 
   
   IF NOT AVAILABLE ibNameOrder THEN RETURN "".
   
   IF ibNameOrder.CustIDType = "CIF" AND ibNameOrder.Company > "" THEN
      RETURN ibNameOrder.Company.

   /* private customers have both lastname and firstname */
   ELSE RETURN ibNameOrder.FirstName + " " + ibNameOrder.SurName1 + 
               (IF ibNameOrder.SurName2 > "" 
                THEN " " + ibNameOrder.SurName2
                ELSE ""). 
    
END FUNCTION.

FUNCTION fWriteMemoWithType RETURNS LOGICAL
   (icHostTable AS CHAR,
    icKeyValue  AS CHAR,
    iiCustNum   AS INT,
    icTitle     AS CHAR,
    icText      AS CHAR,
    icType      AS CHAR,
    icCreUser   AS CHAR).
    
   CREATE Memo.
   ASSIGN Memo.Brand     = gcBrand
          Memo.HostTable = icHostTable
          Memo.KeyValue  = icKeyValue
          Memo.CustNum   = iiCustNum
          Memo.Memotype  = icType
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = icCreUser 
          Memo.MemoTitle = icTitle
          Memo.MemoText  = icText.
          Memo.CreStamp  = fMakeTS().

   RELEASE Memo.
   
   RETURN TRUE. 
   
END FUNCTION.

FUNCTION fWriteMemo RETURNS LOGICAL
   (icHostTable AS CHAR,
    icKeyValue  AS CHAR,
    iiCustNum   AS INT,
    icTitle     AS CHAR,
    icText      AS CHAR).

   RETURN fWriteMemoWithType(icHostTable,icKeyValue,iiCustNum,icTitle,icText,"", katun).
END FUNCTION.


/* transname.i        26.08.08/aam

   get translation names for various items 
*/

&IF "{&TranslationName}" NE "YES" 
&THEN
&GLOBAL-DEFINE TranslationName YES

FUNCTION fTranslationName RETURNS CHAR
  (INPUT icBrand    AS CHAR, 
   INPUT iiItemType AS INT,   /* 1=billitem,2=bdest,3=ccn etc. */
   INPUT icItemKey  AS CHAR,  /* billitem.billcode,bdest.bdest,ccn.ccn etc. */
   INPUT iiLanguage AS INT, 
   INPUT idtDate    AS DATE):

   DEF VAR lcTranslation AS CHAR NO-UNDO INIT ?.

   DO WHILE TRUE:
   
      FOR FIRST RepText NO-LOCK WHERE
                RepText.Brand     = icBrand    AND
                RepText.TextType  = iiItemType AND 
                RepText.LinkCode  = icItemKey  AND
                RepText.Language  = iiLanguage AND
                RepText.ToDate   >= idtDate    AND
                RepText.FromDate <= idtDate:
         lcTranslation = RepText.RepText.
      END.

      /* use basic language if nothing was defined in the desired one */
      IF lcTranslation = ? AND iiLanguage NE 1 THEN DO:
         iiLanguage = 1.
         NEXT.
      END.
      
      LEAVE.
   END.   

   RETURN lcTranslation.

END FUNCTION.

FUNCTION fGetItemName RETURNS CHAR
  (INPUT icBrand    AS CHAR, 
   INPUT icItemName AS CHAR,  /* "billitem","bdest","ccn" etc. */
   INPUT icItemKey  AS CHAR,  /* billitem.billcode,bdest.bdestid,ccn.ccn etc.*/
   INPUT iiLanguage AS INT, 
   INPUT idtDate    AS DATE):

   DEF VAR liItemType AS INT  NO-UNDO.
   DEF VAR lcItemName AS CHAR NO-UNDO.
   DEF VAR liItemKey  AS INT  NO-UNDO.
   
   CASE icItemName:
   WHEN "BillItem"    THEN liItemType = 1.
   WHEN "BDest"       THEN liItemType = 2.
   WHEN "CCN"         THEN liItemType = 3.
   WHEN "Nationality" THEN liItemType = 4.
   WHEN "Country"     THEN liItemType = 5. 
   WHEN "BItemGroup"  THEN liItemType = 6.
   WHEN "TaxZone"     THEN liItemType = 7.
   WHEN "InvSect"     THEN liItemType = 8.
   WHEN "CLIType"     THEN liItemType = 9.
   WHEN "TMSCodes"    THEN liItemType = 10.
   WHEN "RatePlan"    THEN liItemType = 11.
   WHEN "ServPac"     THEN liItemType = 12.
   WHEN "ServCom"     THEN liItemType = 13.
   WHEN "DayCampaign" THEN liItemType = 14.
   /* OrderDelivery table */
   WHEN "LoId"        THEN liItemType = 20.
   WHEN "LoStatusId"  THEN liItemType = 21.
   WHEN "CourierId"   THEN liItemType = 22.
   WHEN "IncidentInfoId" THEN liItemType = 23.
   WHEN "MeasuresInfoId" THEN liItemType = 24.
   WHEN "Profession" THEN liItemType = 25.
   WHEN "BarringCode" THEN liItemType = 26.
   OTHERWISE RETURN "". 
   END CASE.
   
   lcItemName = fTranslationName(icBrand,
                                 liItemType,
                                 icItemKey,
                                 iiLanguage,
                                 idtDate).

   /* get original name if translation is not defined */
   IF lcItemName = ? OR lcItemName = "" THEN 
   CASE liItemType:
   WHEN 1 THEN DO:
      FIND FIRST BillItem WHERE 
                 BillItem.Brand    = icBrand AND
                 BillItem.BillCode = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcItemName = BillItem.BIName.
   END.   
   WHEN 2 THEN DO:
      liItemKey = INT(icItemKey) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
         FIND FIRST BDest WHERE
                    BDest.BDestID = liItemKey NO-LOCK NO-ERROR.
         IF AVAILABLE BDest THEN lcItemName = BDest.BDName.
      END.   
   END.
   WHEN 3 THEN DO:
      liItemKey = INT(icItemKey) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
         FIND FIRST CCN WHERE
                    CCN.Brand = icBrand AND
                    CCN.CCN   = liItemKey NO-LOCK NO-ERROR.
         IF AVAILABLE CCN THEN lcItemName = CCN.CCNName.
      END.   
   END.   
   WHEN 4 THEN DO:
      FIND FIRST Nationality WHERE
                 Nationality.Nationality = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE Nationality THEN lcItemName = Nationality.NtName.
   END.
   WHEN 5 THEN DO:
      FIND FIRST Country WHERE 
                 Country.Country = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE Country THEN lcItemName = Country.CoName.
   END.
   WHEN 6 THEN DO:
      FIND FIRST BItemGroup WHERE
                 BItemGroup.Brand   = icBrand AND
                 BItemGroup.BIGroup = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE BItemGroup THEN lcItemName = BItemGroup.BIGName.
   END.    
   WHEN 7 THEN DO:
      FIND FIRST TaxZone WHERE
                 TaxZone.TaxZone = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE TaxZone THEN lcItemName = TaxZone.TZName.
   END.
   WHEN 8 THEN DO:
      FIND FIRST InvSect WHERE
                 InvSect.Brand   = icBrand AND
                 InvSect.InvSect = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE InvSect THEN lcItemName = InvSect.ISName.
   END.   
   WHEN 9 THEN DO:
      FIND FIRST CLIType WHERE
                 CLIType.Brand   = icBrand AND
                 CLIType.CLIType = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE CLIType THEN lcItemName = CLIType.CLIName.
   END.
   WHEN 10 THEN DO:
      FIND FIRST TMSCodes WHERE
                 TMSCodes.TableName = ENTRY(1,icItemKey,"|") AND
                 TMSCodes.FieldName = ENTRY(2,icItemKey,"|") AND
                 TMSCodes.CodeValue = ENTRY(3,icItemKey,"|") NO-LOCK NO-ERROR.
      IF AVAILABLE TMSCodes THEN lcItemName = TMSCodes.CodeName.
   END.
   WHEN 11 THEN DO:
      FIND FIRST RatePlan WHERE
                 RatePlan.Brand    = icBrand AND
                 RatePlan.RatePlan = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE RatePlan THEN lcItemName = RatePlan.RPName.
   END.
   WHEN 12 THEN DO:
      FIND FIRST ServPac WHERE
                 ServPac.Brand   = icBrand AND
                 ServPac.ServPac = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE ServPac THEN lcItemName = ServPac.SPName.
   END.
   WHEN 13 THEN DO:
      FIND FIRST ServCom WHERE
                 ServCom.Brand   = icBrand AND
                 ServCom.ServCom = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE ServCom THEN lcItemName = ServCom.SCName.
   END.
   WHEN 14 THEN DO:
      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand   = icBrand AND
                 DayCampaign.DCEvent = icItemKey NO-LOCK NO-ERROR.
      IF AVAILABLE DayCampaign THEN lcItemName = DayCampaign.DCName.           
   END.
   END CASE.

   IF lcItemName = ? THEN lcItemName = "".
   
   RETURN lcItemName.
    
END FUNCTION.
                           
&ENDIF



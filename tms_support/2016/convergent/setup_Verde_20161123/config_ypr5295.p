DEF VAR lcName AS CHAR NO-UNDO.
DEF VAR ldaFrom AS DATE INIT TODAY.

DEF TEMP-TABLE ttRequestAction NO-UNDO LIKE RequestAction.
DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttTariff NO-UNDO LIKE Tariff.
DEF BUFFER bClitype FOR Clitype.

/**Generic functions*/
DEF VAR liExec AS INT NO-UNDO.
liExec = 1.
/*IF liExec NE 0 THEN RETURN.*/


FUNCTION fServiceOfClitype RETURNS LOGICAL
   (icSPac AS CHAR,
    icClitype AS CHAR):
   DEF BUFFER ctservpac FOR ctservpac.
   FIND FIRST ctservpac NO-LOCK where
              ctservpac.brand EQ "1" AND
              ctservpac.clitype EQ icClitype AND
              ctservpac.servpac EQ icSPac AND
              ctservpac.todate > TODAY NO-ERROR.
   IF AVAIL ctservpac THEN RETURN TRUE.

   RETURN FALSE.

END.


FUNCTION fCleanServiceElement RETURNS LOGICAL
    (icClitype AS CHAR,
     icServPac AS CHAR):
   DEF BUFFER ctservel FOR ctservel.
   FOR EACH ctservel where
            ctservel.brand EQ "1" AND
            ctservel.clitype EQ icClitype AND
            ctservel.servpac EQ icServPac:
/*      disp "removing service element".
      disp ctservel.clitype FORMAT "X(15)".
      disp ctservel.servpac FORMAT "x(22)".
      disp ctservel.servcom.*/
      IF liExec NE 0 THEN DELETE ctservel.
   END.
END.



/*function removes sertvices that are not available for CONTDSL45*/
FUNCTION fCleanServicePac RETURNS LOGICAL
    (icClitype AS CHAR):
   DEF BUFFER ctservpac FOR ctservpac.
   FOR EACH ctservpac where
            ctservpac.brand EQ "1" AND
            ctservpac.clitype EQ icClitype:
      /*disp ctservpac.clitype FORMAT "X(15)".
      disp ctservpac.servpac FORMAT "x(22)".*/
      IF fServiceOfClitype(ctservpac.servpac, "CONTDSL45") EQ TRUE THEN DO:
         /*disp "do not remove".*/
      END.
      ELSE DO:
         /*disp "remove".*/
         fCleanServiceElement(ctservpac.clitype, ctservpac.servpac).
         IF liExec NE 0 THEN DELETE ctservpac.
      END.
   END.
END.



FUNCTION create_ra returns log(INPUT icBasetype AS CHAR,
                    INPUT icClitype AS CHAR,
                    INPUT iiUpdateMode AS INT):
   DEF VAR liActionID AS INT NO-UNDO.
   DEF BUFFER bRequestAction FOR RequestAction.

   FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
      IF AVAILABLE RequestAction THEN
         liActionID = RequestAction.RequestActionID + 1.
      ELSE liActionID = 1.
   FOR EACH bRequestAction WHERE
            bRequestAction.clitype EQ icBaseType:
      CREATE ttRequestAction.
      BUFFER-COPY bRequestAction TO ttRequestAction.
      ASSIGN
      ttRequestAction.ValidFrom = ldaFrom
      ttRequestAction.clitype = icCliType
      ttRequestAction.RequestActionID = liActionID.
      IF  ttRequestaction.actionkey EQ icBasetype THEN
         ttRequestaction.actionkey = icclitype.
      IF (icClitype BEGINS "CONTDSL" OR icClitype BEGINS "CONTFH") AND
          bRequestAction.reqtype EQ 13 THEN DO:
         ttRequestAction.Reqtype = 14.
         IF ttRequestaction.actionkey BEGINS "CONTDSL" THEN
            ttRequestaction.actionkey = "CONTDSL".
         ELSE IF ttRequestaction.actionkey BEGINS "CONTFH" THEN DO:
            IF INDEX(ttRequestaction.actionkey, "50") > 0 THEN
               ttRequestaction.actionkey = "CONTFH50".
            ELSE IF INDEX(ttRequestaction.actionkey,"300") > 0 THEN
               ttRequestaction.actionkey = "CONTFH300".
         END.
      END.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE RequestAction.
         BUFFER-COPY ttRequestAction TO RequestAction.
      END.
      ELSE DISP ttRequestAction.
      DELETE ttRequestAction. /*ror safety reasons*/
      liActionID = liActionID + 1.
   END.

END.

FUNCTION create_ra_mob returns log(INPUT icBasetype AS CHAR,
                                   INPUT icClitype AS CHAR,
                                   INPUT ickey AS CHAR,
                                   INPUT iiUpdateMode AS INT):
   DEF VAR liActionID AS INT NO-UNDO.
   DEF BUFFER bRequestAction FOR RequestAction.

   FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
      IF AVAILABLE RequestAction THEN
         liActionID = RequestAction.RequestActionID + 1.
      ELSE liActionID = 1.
   FIND FIRST bRequestAction WHERE
              bRequestAction.clitype EQ icBaseType AND
              bRequestAction.reqtype = 13 NO-ERROR.
      CREATE ttRequestAction.
      BUFFER-COPY bRequestAction TO ttRequestAction.
      ASSIGN
      ttRequestAction.ValidFrom = ldaFrom
      ttRequestAction.clitype = icCliType
      ttRequestAction.RequestActionID = liActionID.
      ttRequestaction.actionkey = ickey.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE RequestAction.
         BUFFER-COPY ttRequestAction TO RequestAction.
      END.
      ELSE DISP ttRequestAction.
      DELETE ttRequestAction. /*ror safety reasons*/
      liActionID = liActionID + 1.


END.


DEF VAR liSeq AS INT.

FUNCTION create_matrix returns int(
   input ickey AS CHAR,
   input icName AS CHAR):

   DEF VAR liPriority AS INT.
   FIND LAST Matrix WHERE
             Matrix.mxkey EQ icKey.

   liPriority = Matrix.prior + 1.

   CREATE Matrix.
   ASSIGN
      Matrix.Brand  = "1"
      Matrix.MXSeq  = NEXT-VALUE(imsi)
      Matrix.mxkey  = icKey
      Matrix.mxname = icName
      Matrix.prior  = liPriority
      Matrix.mxres  = 1.
   RETURN Matrix.MXSeq.
END.


FUNCTION create_mxitem returns int (input icname AS CHAR,
                                    input icValue AS CHAR,
                                    input iiseq AS INT):
   CREATE MXItem.
   ASSIGN
      MXItem.mxseq = iiseq
      MXItem.mxvalue = icValue
      MXItem.mxname = icname.
END.

/*EXECUTION PART*/

/*matrix*/
lcName = "Convergent CONTFH50".
FIND FIRST matrix NO-LOCK where 
           matrix.mxname EQ lcName.
IF NOT AVAIL matrix THEN DO:
   MESSAGE "Matrix not found: " + lcName VIEW-AS ALERT-BOX.
   RETURN.
END.
ELSE DO:
   create_mxitem("SubsTypeTo","CONTFH48_50",matrix.MxSeq).
END.
RELEASE matrix.

lcName = "Convergent CONTFH300".
FIND FIRST matrix NO-LOCK where 
           matrix.mxname EQ lcName.
IF NOT AVAIL matrix THEN DO:
   MESSAGE "Matrix not found: " + lcName VIEW-AS ALERT-BOX.
   RETURN.
END.
ELSE DO:
   create_mxitem("SubsTypeTo","CONTFH58_300",matrix.MxSeq).
END.
RELEASE matrix.

/*Mobile part matrix*/

liSeq = create_matrix("PERCONTR", "Convergent 5GB  mobile").
create_mxitem("PerContract","CONT15",liSeq).
create_mxitem("PerContract","DATA6",liSeq).
create_mxitem("SubsTypeTo","CONTDSL48",liSeq).
create_mxitem("SubsTypeTo","CONTFH48_50",liSeq).
create_mxitem("SubsTypeTo","CONTFH58_300",liSeq).

/*ac13*/
create_mxitem("PerContract","MDUB3",liSeq). /*bono25*/
create_mxitem("PerContract","MDUB4",liSeq). /*bono35*/
create_mxitem("PerContract","DATA7",liSeq).



/*tmsparam*/
FIND FIRST TMSParam WHERE TMSParam.ParamCode EQ "DATA_BUNDLE_BASED_CLITYPES"
   NO-ERROR.

IF LOOKUP("CONTDSL48", TMSParam.charval) = 0 THEN
TMSParam.charval = tmsParam.charval + ",CONTDSL48,CONTFH48_50," +
                   "CONTFH58_300".

FIND FIRST TMSParam WHERE TMSParam.ParamCode EQ "POSTPAID_VOICE_TARIFFS"
   NO-ERROR.

IF LOOKUP("CONTDSL48", TMSParam.charval) = 0 THEN
   TMSParam.charval = tmsParam.charval + ",CONTDSL48,CONTFH48_50," +
                      "CONTFH58_300".



/*tmscodes - no need to do*/

/*requestaction*/
         /*BASE        TYPE*/
create_ra("CONTDSL45","CONTDSL48",liExec).
create_ra("CONTFH45_50","CONTFH48_50",liExec).
create_ra("CONTFH55_300","CONTFH58_300",liExec).

create_ra_mob("CONTDSL45","CONTDSL48","CONT15",liExec).
create_ra_mob("CONTFH45_50","CONTFH48_50","CONT15",liExec).
create_ra_mob("CONTFH55_300","CONTFH58_300","CONT15",liExec).

/*clean services */
fCleanServicePac("CONTDSL48").
fCleanServicePac("CONTFH48_50").
fCleanServicePac("CONTFH58_300").


FOR EACH bCliType WHERE
         bClitype.brand EQ "1" AND
         bClitype.clitype BEGINS "CONTDSL":
   FIND FIRST CliType WHERE
         Clitype.brand EQ "1" AND
         Clitype.clitype EQ bClitype.clitype  AND
         Clitype.fixedlinetype EQ 1 NO-ERROR.
   IF NOT AVAIL Clitype THEN DO:
      ASSIGN
      bClitype.fixedlinetype = 1
      bClitype.FixedLineDownload = "20M"
      bClitype.FixedLineUpload = "20M".
   END.
END.



FOR EACH bCliType WHERE
         bClitype.brand EQ "1" AND
         bClitype.clitype MATCHES "CONTFH*50":
   FIND FIRST CliType WHERE
         Clitype.brand EQ "1" AND
         Clitype.clitype EQ bClitype.clitype  AND
         Clitype.fixedlinetype EQ 2 NO-ERROR.
   IF NOT AVAIL Clitype THEN DO:
      ASSIGN
      bClitype.fixedlinetype = 2
      bClitype.FixedLineDownload = "50M"
      bClitype.FixedLineUpload = "5M".

   END.
END.

FOR EACH bCliType WHERE
         bClitype.brand EQ "1" AND
         bClitype.clitype MATCHES "CONTFH*300":
   FIND FIRST CliType WHERE
         Clitype.brand EQ "1" AND
         Clitype.clitype EQ bClitype.clitype  AND
         Clitype.fixedlinetype EQ 2 NO-ERROR.
   IF NOT AVAIL Clitype THEN DO:
      ASSIGN
      bClitype.fixedlinetype = 2
      bClitype.FixedLineDownload = "300M"
      bClitype.FixedLineUpload = "300M".
   END.
END.

DEF TEMP-TABLE ttCTServEl NO-UNDO LIKE CTServEl.
/*
         FOR EACH CTServEl WHERE
                  CTServEl.Brand   = "1"          AND
                  CTServEl.CLIType = "CONTDSL58" AND
                  CTServEl.ServPac = "TMSService" NO-LOCK:
    DISP CTServEl.
END.
*/
FUNCTION faddServEl RETURNS LOGICAL (INPUT icCliType AS CHAR,
                                     INPUT icParam AS CHAR):
FIND FIRST CTServEl WHERE
           CTServEl.Brand   = "1"          AND
           CTServEl.CLIType = icCliType AND
           CTServEl.ServPac = "TMSService" AND
           CTServEl.servcom = "SHAPER_STP" NO-ERROR. 

IF NOT AVAIL CTServEl THEN DO:
   FIND FIRST CTServEl WHERE
              CTServEl.Brand   = "1"          AND
              CTServEl.CLIType = "CONT24" AND /*only for base, to be replaced*/
              CTServEl.ServPac = "TMSService" AND
              CTServEl.servcom = "SHAPER_STP" NO-ERROR.

   BUFFER-COPY CTServEl EXCEPT CTServEl.CTServEl
                               CTServEl.CLIType
                               CTServEl.FromDate
                               TO ttCTServEl.
   ASSIGN ttCTServEl.CTServEl = NEXT-VALUE(CTServEl)
          ttCTServEl.CLIType  = icCliType
          ttCTServEl.defparam = icParam
          ttCTServEl.FromDate = TODAY NO-ERROR.

   CREATE CTServEl.
   BUFFER-COPY ttCTServEl TO CTServEl.
END.
END.

fAddServEl("CONTDSL48","CONT15").
fAddServEl("CONTFH48_50","CONT15").
fAddServEl("CONTFH58_300","CONT15").

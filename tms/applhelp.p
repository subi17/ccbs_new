/*-----------------------------------------------
  MODULE .......: applhelp.p
  KUTSUVAMODULI : mikA vain
  FUNCTION .....: helpin pAAmoduli
  APPLICATION ..: TyOnhallinta
  AUTHOR .......: PT
  CREATED ......: 17.06.1990
  ------------------------------------------------------------------------ */

{Syst/commali.i}

def new shared var siirto as char format "x(75)".
DEF VAR i AS INT NO-UNDO.
DEF VAR save-ehto LIKE ehto NO-UNDO.
DEF VAR save-ufk LIKE ufk   NO-UNDO.
DEF VAR hmod AS c NO-UNDO.
DEF VAR pp AS INT NO-UNDO.
DEF VAR kk AS INT NO-UNDO.
DEF VAR vv AS INT NO-UNDO.
DEF VAR df AS c   NO-UNDO.
DEF VAR lcCode AS CHAR NO-UNDO.


/* talteen ufKey:n common-arvot jottei sotketa paikkoja */
save-ehto = ehto.
DO i = 1 TO 9 :
   save-ufk[i] = ufk[i].
END.
helpkey = keylabel(LASTKEY).

gcHelpParam = "ahelp".

if helpkey = "f31" THEN RUN istrans.p.
else if helpkey = "f10" THEN DO:
END.
ELSE DO:

/* ----------------------------------------------------------------- */
if lookup(frame-field,"InvCust,RateCust,RepCust,PaymCust,AgrCust") > 0 OR
   lookup(frame-field,"custnr,cust-no1,cust-no2") > 0 OR
   index(frame-field,"CustNum") > 0 OR
   index(frame-field,"cust-nr") > 0 OR
   index(frame-field,"asno") > 0    OR
   index(frame-field,"custno") > 0 

   THEN DO:
   RUN Mc/nnasel.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"BillCode") > 0  OR
        index(frame-field,"BillItem") > 0  OR 
        index(frame-field,"ServiceLMember") > 0
THEN DO:
     RUN Help/nntuse.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"Target") > 0 THEN 
DO:
   RUN Help/targets.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"gcbrand,Brand") > 0 OR
     index(frame-field,"brand") > 0 
THEN DO:
   RUN Help/h-brand.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"BankId") > 0 THEN
DO:
   RUN Mc/bankid.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"Parent") > 0 OR
        index(frame-field,"salesman") > 0 OR
        index(frame-field,"myyja") > 0 OR
        index(frame-field,"sm-code") > 0 THEN DO:
     RUN Help/nnmyse.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"SalesOffice") > 0 THEN DO:
     RUN Help/nnsose.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"Reseller") > 0 THEN DO:
     RUN Mc/nnrsse.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"Language,kie,CCLang") > 0 THEN DO:
     RUN Help/h-language.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"AccType") > 0 THEN DO:
     RUN Help/h-acyp.p.
     IF siirto <> ? THEN frame-value = siirto.
END.


else if lookup(frame-field,"xug-code") > 0  then do:
   RUN Help/nnugse.p.
   if siirto <> ? then frame-value = siirto.
end.


else if lookup(frame-field,"RepCodes") > 0 THEN DO:
     RUN Help/h-repcode.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if 
     index(frame-field,"BDest") > 0 THEN DO:
     RUN Mc/nnbtse.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"Category") > 0 THEN DO:
     RUN Mc/nnakse.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if 
        index(frame-field,"pvm") > 0 OR   
        index(frame-field,"date") > 0 OR
        index(frame-field,"day") > 0 OR
        lookup(frame-field,"ValidFrom,ValidTo") > 0 THEN DO:

     if index(frame-value," ") > 0 or frame-value = "" THEN DO:
        ASSIGN pp = day(pvm) kk = month(pvm) vv = year(pvm).
     END.
     ELSE DO:
        ASSIGN
           pp = integer(substring(frame-value,1,2))
           kk = integer(substring(frame-value,4,2)).
        IF length(frame-value) = 8 THEN ASSIGN
           vv = integer(substring(frame-value,7,2)).
        ELSE ASSIGN
           vv = integer(substring(frame-value,7,4)).
     END.

     IF vv < 100 THEN DO:
        IF vv > 80 THEN vv = vv + 1900.
        ELSE            vv = vv + 2000.
     END.

     i = index(df," ").
     DO WHILE i > 0.
        substr(df,i,1) = "".
        i = index(df," ").
     END.

     /* check the FORMAT of the frame-field */
     si-pvm = date(kk,pp,vv).
     frame-value = string(si-pvm,"99-99-9999").
     IF length(frame-value) = 8 THEN ASSIGN
        df = "99-99-99".
     ELSE ASSIGN
        df = "99-99-9999".
     frame-value = string(si-pvm,df).

     RUN Syst/ukale.p.
     IF si-pvm <> ? THEN DO:
        PAUSE 0 no-message.
        frame-value = string(si-pvm,df).
     END.
     ELSE si-pvm = TODAY.
END.
ELSE IF lookup(frame-field,"UserCode") 
    > 0 THEN DO:
     RUN Syst/uktse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"UserName")
    > 0 THEN DO:
     RUN uknse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"MenuClass") > 0 THEN DO:
     RUN Help/nnpcse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if index(frame-field,"InvNum") > 0  THEN DO:
     RUN Ar/nnlase.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

ELSE IF INDEX(FRAME-FIELD,"ExtInvID") > 0 THEN DO:
   RUN Ar/nnlase.p. 
   IF siirto NE ? THEN DO:
      FIND Invoice WHERE Invoice.InvNum = INTEGER(siirto) NO-LOCK NO-ERROR.
      IF AVAILABLE Invoice THEN siirto = Invoice.ExtInvID.
      FRAME-VALUE = siirto.
   END.
END.

else if lookup(frame-field,"OpCode,Operator,h-op-code") > 0  THEN DO:
     RUN Help/nnopse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if index(frame-field,"ExCode") > 0   OR
        index(frame-field,"ex-code") > 0   
THEN DO:
     RUN Mf/nnexse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if index(frame-field,"TrunkCode") > 0 OR
        index(frame-field,"TrunkName") > 0 THEN DO:
     RUN Mf/nntrse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"PriceList,xPriceList") > 0  THEN DO:
     RUN Help/nnplse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if index(frame-field,"InvGroup") > 0 OR
        index(frame-field,"ig-code") > 0
THEN DO:
     RUN Mc/nnigse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"custgroup,xCustGroup,xcg-code") > 0  THEN DO:
     RUN Mc/nncgse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.



else if lookup(frame-field,"SmGroup,xSmGroup,xg-code,mry1,mry2") > 0  THEN DO:
     RUN Help/nnsgse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

ELSE IF INDEX(frame-field,"areacode") > 0 OR
        LOOKUP(frame-field,"neigarea") > 0

THEN DO:
     RUN Mf/nnrnse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"xb-code,xb-name") > 0  THEN DO:
     RUN nnxbse2.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"TrafficArea") > 0  THEN DO:
     RUN Mf/nnsnse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

/* --------------------- menu-tekstiBROWSE ------------------------- */
else if lookup(frame-field,"MenuNum"
/*
va-tno,v#10,v#11,v#12,v#15,v#16"
*/
) > 0 THEN DO:
   siirto = frame-value.
   RUN Syst/umese.p.
   IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"PrinterId") > 0  THEN DO:
     RUN Syst/ukirse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"BIGroup,lcGraph") > 0 THEN DO:
     RUN Help/nnpgse.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.
else if index(frame-field,"AccNum") > 0  OR 
     index(frame-field,"Acc") > 0   THEN DO:
     RUN Mc/nnacse.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"repccn,ccn,hakumaa") > 0 OR
     INDEX(frame-field,"ccn") > 0
THEN DO:
     RUN Help/nnmase.p.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"cc-code") > 0 THEN DO:
     RUN nnccse.
     IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if lookup(frame-field,"Ident") > 0 THEN DO:
     RUN Mf/nnidse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"Desttype") > 0 THEN DO:
   RUN Help/h-tmscodes.p(INPUT "BDest",  /* TableName*/
                          "BDestType", /* FieldName */
                          ?, /* GroupCode */
                    OUTPUT lcCode).

   IF lcCode ne "" AND lcCode NE ? THEN FRAME-VALUE = lcCode.
END.

else if lookup(frame-field,"Currency") > 0 THEN DO:
     RUN Help/nncuse.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if index(frame-field,"PaymSrc") > 0 then do:
     RUN Help/h-paymsrc.p.
     if siirto <> ? then frame-value = siirto.
end.

else if index(frame-field,"DialType") > 0  THEN DO:
     RUN Help/h-dialtype.p.
     IF siirto <> ? THEN frame-value = siirto.
END.

else if lookup(frame-field,"UserGroup,UserGrp") > 0 THEN DO:
     RUN Help/hugroup.p.
     IF siirto NE ? THEN frame-value = siirto.

END.

else if lookup(frame-field,"tokencode") > 0 THEN DO:
    RUN Help/htoken.p.
    if siirto ne ? then frame-value = siirto.
END.

else if index(frame-field,"contract") > 0 and
     si-recid2 > 0 and si-recid2 ne ?
then do:
   RUN Help/h-contract.p(si-recid2).
   if siirto <> ? then frame-value = siirto.
end.

else if index(frame-field,"CostCentre") > 0 THEN DO:
   RUN Mc/ccentre.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"dcevent") > 0 THEN DO:
   RUN Help/h-daycamp.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"Region") > 0 THEN DO:
   RUN Help/h-region.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"TaxZone") > 0 THEN DO:
   RUN Help/h-taxzone.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"TaxClass") > 0 THEN DO:
   RUN Help/h-taxclass.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"Nationality") > 0 THEN DO:
   RUN Help/h-nationality.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

else if index(frame-field,"ZipCode") > 0 OR
        INDEX(FRAME-FIELD,"PostCode") > 0 
THEN DO:
   RUN Help/h-postcode.p.
   IF siirto NE ? THEN ASSIGN frame-value = siirto.
END.

ELSE IF INDEX(FRAME-FIELD,"BRTestCase") > 0 THEN DO:
   RUN Inv/brtestcase.p.
   IF siirto NE ? THEN FRAME-VALUE = siirto.
END.

ELSE IF INDEX(FRAME-FIELD,"DPRuleID") > 0 THEN DO:
   RUN Mc/discountplan.p.
   IF siirto NE ? THEN ASSIGN FRAME-VALUE = siirto.
END.

ELSE DO: /* Mobile-related tables ... */

 hmod = ?.  /* default: unknown FIELD */

 if lookup(frame-field,"discplan") > 0                then hmod = "Help/h-dplan".
 else if lookup(frame-field,"rateplan,PNPRatePlan,priceplan") > 0 
                                                      then hmod = "Help/h-rplan". 
 else if lookup(frame-field,"PNPGroup") > 0           then hmod = "Help/hpnpgrp".
 else if lookup(frame-field,"mancode") > 0            then hmod = "Help/h-simman".
 else if lookup(frame-field,"simstat,new-simstat") > 0
                                                      then hmod = "Help/h-simstat".
 else if lookup(frame-field,"statuscode,simstat,simstat1,simstat2") > 0  
                                                      then hmod = "Help/h-msstat".
 else if lookup(frame-field,"simart") > 0             then hmod = "Help/h-simart".
 else if lookup(frame-field,"beacap") > 0             then hmod = "Help/h-beacap".
 else if lookup(frame-field,"Stock,stock1,stock2,stobal") > 0   
                                                      then hmod = "Help/h-stock".
 else if lookup(frame-field,"servel") > 0             then hmod = "Help/h-servic".
 else if lookup(frame-field,"servpac,servicepack,def-sp-code") > 0 OR
         INDEX(FRAME-FIELD,"ServPac") > 0             then hmod = "Help/h-servpa".
 else if index(frame-field,"country") > 0            then hmod = "Help/h-count".
 else if lookup(frame-field,"mccode,mccode1,mccode2") > 0   
                                                      then hmod = "Help/h-msclas".
 else if lookup(frame-field,"icc,icc1,icc2,new-icc") > 0       
                                                      then hmod = "Help/h-sim".
 else if index(frame-field,"servcom") > 0             then hmod = "Help/h-servco".
 else if lookup(frame-field,"bservcom") > 0   
                                                      then hmod = "Help/h-servcob".

 else if lookup(frame-field,"cli,cli1,cli2,new-cli,new-cli-end,xcli,lowlimit,hilimit") > 0 
                                                      then hmod = "Help/h-msisdn".
 else if lookup(frame-field,"discgroup") > 0          then hmod = "Help/h-discgrp".
 else if lookup(frame-field,"moberror,errorcode,moberror1,moberror2") > 0   
                                                      then hmod = "Help/h-mrerr".
 else if lookup(frame-field,"penaltyfee") > 0 or
         index(frame-field,"feemodel") > 0            then hmod = "Help/h-bevent".
 else if lookup(frame-field,"invsect") > 0            then hmod = "Help/h-invsect".
 else if lookup(frame-field,"epgroup") > 0            then hmod = "Help/h-epgrp".
 else if lookup(frame-field,"vatcode") > 0            then hmod = "Help/h-vatcode".
 else if lookup(frame-field,"billtype") > 0           then hmod = "Help/h-obity". 
 else if index (frame-field,"CLItype") > 0            then hmod = "Help/h-mobtype". 
 else if lookup(frame-field,"templnum") > 0           then hmod = "Help/h-custemp".
 else if lookup(frame-field,"Product") > 0            then hmod = "Help/h-product".
 else if lookup(frame-field,"ProdPack") >0            then hmod = "Help/h-prodpack".
 else if lookup(frame-field,"Fatgroup,fatgrp,ftgrp") >0 or
<<<<<<< HEAD:tms/applhelp.p
      index(frame-field,"FatGroup") > 0               then hmod = "Help/h-fatgroup".
 else if lookup(frame-field,"paraname") >0     then hmod = "Help/h-pdpid".
 else if lookup(frame-field,"msstatus") > 0    then hmod = "Help/h-mobsubstatus".
 else if index(frame-field,"servicelimit") > 0 then hmod = "Help/h-servlimitgrp".
 else if index(frame-field,"plmn") > 0         then hmod = "Help/h-roamoper".
 else if index(frame-field,"RoamGroup") > 0    then hmod = "Help/h-roamgroup".
 ELSE IF INDEX(FRAME-FIELD,"ReqStat") > 0      THEN hmod = "Help/h-reqstat".
=======
      index(frame-field,"FatGroup") > 0               then hmod = "h-fatgroup".
 else if lookup(frame-field,"paraname") >0     then hmod = "h-pdpid".
 else if lookup(frame-field,"msstatus") > 0    then hmod = "h-mobsubstatus".
 else if index(frame-field,"servicelimit") > 0 then hmod = "h-servlimitgrp".
 ELSE IF INDEX(FRAME-FIELD,"ReqStat") > 0      THEN hmod = "h-reqstat".
>>>>>>> origin/master:tms/Help/applhelp.p

 if hmod ne "" AND hmod NE ? THEN DO:
    RUN VALUE(hmod).
    IF siirto NE ? THEN frame-value = siirto.
 END.
END.    

IF hmod = ? THEN DO: /* no match FOR frame-field */
     BELL.
     PAUSE 0.
     DISPLAY skip(1)
             " There is no help browser available " SKIP
             " for this case / field"          skip(1)
             " (field " + frame-field + ")" format "x(24)" SKIP
             " PRESS ENTER"        skip(1)
     WITH 
     overlay frame nohelp title " SORRY ... " centered ROW 6
     NO-LABELS COLOR messages.
     PAUSE no-message.
     HIDE FRAME nohelp no-pause.
END.

/* ------------------------------------------------- */
END.

gcHelpParam = "".

/* ja lopuksi palautetaan ufKey:n arvot */

ehto = save-ehto.
DO i = 1 TO 9:
   ufk[i] = save-ufk[i].
END.
PAUSE 0.   /* KJ�H KJ�H */
RUN Syst/ufkey.p.
PAUSE 0.


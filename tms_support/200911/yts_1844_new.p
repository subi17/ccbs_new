


{commpaa.i}
katun = "rafaeldv".
gcbrand = "1".
{service.i}

DEFINE STREAM sout.
DEFINE VARIABLE iDispInterval AS INTEGER NO-UNDO INITIAL 1000.
DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liNumComp AS INTEGER NO-UNDO.
DEFINE VARIABLE liNumUp AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNumForb AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNumOff AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO.

ldTS = fHMS2TS( DATE(11,4,2009),
                "08:00:00").
FIND FIRST ServPac WHERE
           ServPac.ServPac = "OI_SMSB20" NO-LOCK NO-ERROR.

OUTPUT STREAM sout TO fix_new_smsbundle2.log . 

FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand = gcBrand AND
         LOOKUP(MobSub.CliType,"TARJ,TARJ4") > 0 AND
         MobSub.ActivationTS > ldTS AND
         Mobsub.ActivationTS < 20091123.00000  AND 
         MobSub.MsStatus = 4 :

   /* pick subscription creation Order */ 
   FIND FIRST Order WHERE
              Order.Msseq = MobSub.MsSeq AND
              Order.OrderType NE 2 NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN NEXT.

   /* must be a new order */
   IF Order.MNPStatus NE 0 THEN NEXT. 

   /* activation Stamp must after deployment */
   IF Order.CrStamp < ldTS THEN DO:
      liNumForb = liNumForb + 1.
      NEXT.
   END.
   /* skip those that exist */
   FIND FIRST SubSer WHERE
              SubSer.MsSeq = MobSub.MsSeq AND
              SubSer.ServCom = "SMSBUNDLE" NO-LOCK NO-ERROR.
   IF AVAIL SubSer THEN NEXT. 
   
   /* create the smsbundle */
   liNumComp = 0.
   
   RUN pCopyPackage(MobSub.CLIType,
                    ServPac.ServPac, /* servpac */
                    MobSub.MSSeq,
                    MobSub.ActivationDate,
                    ?,    /* all changed ones, force it  */
                    FALSE,   /*  create fees */
                    TRUE,   /* solog (provisioning) */
                    OUTPUT liNumComp).
    
   IF liNumComp = 0 THEN DO:
   END.

   /* DO Statistic */
   liNumUp = liNumUp + 1 .
   /* special case of offer TS000000TVTVP0 */ 
   IF Order.Offer EQ "TS000000TVTVP0" THEN 
   liNumOff = liNumOff + 1 .
   /* display data */
   lcOut = "CLI: " + MobSub.CLI + " " + STRING(MobSub.CreationDate) + " OrderId:" + STRING(Order.OrderId) .
   PUT STREAM sout UNFORMATTED lcOut SKIP. 

   iCount = iCount + 1.
   IF iDispInterval > 0 AND iCount MOD iDispInterval EQ 0 THEN DISP iCount.

END.

lcOut = "Updated: " + STRING(liNumUp) +  " by TS000000TVTVP0 : " + STRING(liNumOff) + " Forbiden: " + STRING(liNumForb) .
PUT STREAM sout UNFORMATTED lcOut SKIP. 


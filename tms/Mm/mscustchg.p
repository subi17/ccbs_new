/*-----------------------------------------------------------------------------
  MODULE .......: mscustchg.p
  FUNCTION .....: handle requests for mobsub customer changes
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 10.01.06
  CHANGED.. ....: 27.03.06/aam separated from msrequest.i
  Version ......: M15
  -------------------------------------------------------------------------- */


{msreqfunc.i}
{eventval.i}
{fwebuser.i}
{coinv.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
   lhMobSub = BUFFER MobSub:HANDLE.
   RUN StarEventInitialize(lhMobSub).

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).

END.


DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN RETURN "ERROR".

CASE MsRequest.ReqType:
/* user / invcust change */
WHEN 3 THEN DO:
   lcReqType = "ucreq".
   RUN pMsCustChange.
END.

/* user / invcust change */
WHEN 4 THEN DO:
   lcReqType = "icreq".
   RUN pMsCustChange.
END.

/* agreement customer change */
WHEN 10 THEN DO:
   lcReqType = "owreq".
   RUN pOwnerChange.
END.

OTHERWISE RETURN "ERROR".
END CASE.

/* clean out dynamic temp-tables (eventlog) */
fCleanEventObjects().

RETURN RETURN-VALUE.


/* user / invcust change */
PROCEDURE pMsCustChange:

   DEF VAR liNewUser    AS INT  NO-UNDO.
   DEF VAR liFeePeriod  AS INT  NO-UNDO. 
   DEF VAR liDefCust    AS INT  NO-UNDO. 
   DEF VAR ldEndStamp   AS DEC  NO-UNDO.
   DEF VAR ldtFeeFrom   AS DATE NO-UNDO.
   DEF VAR ldtFeeTo     AS DATE NO-UNDO. 
   DEF VAR ldFeeAmt     AS DEC  NO-UNDO. 
   DEF VAR liPerDays    AS INT  NO-UNDO.
   DEF VAR ldtFeeDate   AS DATE NO-UNDO.
   DEF VAR liNewTarget  AS INT  NO-UNDO.
   DEF VAR liNewInvCust AS INT  NO-UNDO.
   DEF VAR liUserCust   AS INT  NO-UNDO.
   DEF VAR liCreated    AS INT  NO-UNDO.
   DEF VAR liOldUser    AS INT  NO-UNDO.
   DEF VAR liOldInvCust AS INT  NO-UNDO.

   DEF BUFFER bNewCust    FOR Customer.
   DEF BUFFER bBillTarget FOR BillTarget.
   DEF BUFFER bOwner      FOR MSOwner.
   DEF BUFFER bOMobSub    FOR MobSub.
   DEF BUFFER bFixedFee   FOR FixedFee.
   DEF BUFFER bFFItem     FOR FFItem.
        
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   /* request type 3 = user change, 4 = inv.customer change, 
      10 = agr.customer change */
   
   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("MobSub not found").
      RETURN.
   END.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq NO-LOCK.
   IF NOT AVAILABLE MsOwner OR MsOwner.CustNum NE MobSub.CustNum OR
      MsOwner.TSEnd < MsRequest.ActStamp
   THEN DO:
      fReqError("Invalid MSOwner data").
      RETURN.
   END. 
 
   /* nor an existing customer nbr or a name for the new customer is given */
   IF (MsRequest.ReqType    = 3 AND
       MsRequest.ReqIParam1 = 0 AND 
       MsRequest.ReqCParam1 = "")   OR
      (MsRequest.ReqType    = 4 AND
       MsRequest.ReqIParam2 = 0 AND 
       MsRequest.ReqCParam1 = "")   
   THEN DO:
      fReqError("Nothing to do").
      RETURN. 
   END.

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
 
   ASSIGN liNewUser    = MsRequest.ReqIParam1
          liNewInvCust = MsRequest.ReqIParam2
          liOldUser    = MobSub.CustNum
          liOldInvCust = MobSub.InvCust
          liCreated    = 0.
   
   /* a new customer will be created */
   IF (MsRequest.ReqType = 3 AND liNewUser = 0) OR
      (MsRequest.ReqType = 4 AND liNewInvCust = 0)
   THEN DO:

      /* group from invoicing customer */
      IF liNewInvCust > 0 
      THEN FIND Customer WHERE Customer.CustNum = liNewInvCust 
              NO-LOCK NO-ERROR.
      /* use current one's inv.group if a new inv.cust is created */  
      ELSE FIND Customer WHERE Customer.CustNum = MobSub.InvCust 
              NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE Customer THEN DO:
         fReqError("Unknown invoicing customer").
         RETURN.
      END. 
         
      liDefCust = fCParamI("DefCust" + Customer.InvGroup + "/" +
                           ENTRY(1,MsRequest.ReqCParam4,";")).
      
      IF liDefCust = ? OR liDefCust = 0 THEN DO:
         fReqError("Default customer not defined for " + Customer.InvGroup).
         RETURN.
      END. 
      
      liCreated = liDefCust.

      RUN copymobcu (INPUT-OUTPUT liCreated,
                     FALSE).
      
      FIND bNewCust WHERE bNewCust.CustNum = liCreated EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bNewCust THEN DO:
      
         IF liNewUser    = 0 THEN liNewUser    = liCreated.
         IF liNewInvCust = 0 THEN liNewInvCust = liCreated.
            
         ASSIGN bNewCust.ChgStamp   = fMakeTS()
                bNewCust.CreUser    = katun
                bNewCust.InvCust    = liNewInvCust
                bNewCust.PaymCust   = MobSub.AgrCust
                bNewCust.RepCust    = bNewCust.CustNum
                bNewCust.RateCust   = bNewCust.CustNum 
                bNewCust.AgrCust    = MobSub.AgrCust
                bNewCust.CustName   = ENTRY(1,MsRequest.ReqCParam1,";")
                bNewCust.FirstName  = ENTRY(2,MsRequest.ReqCParam1,";")
                bNewCust.SearchName = SUBSTRING(bNewCust.CustName + " " + 
                                                bNewCust.FirstName,1,8)
                bNewCust.COName     = ENTRY(3,MsRequest.ReqCParam1,";")
                bNewCust.Address    = ENTRY(1,MsRequest.ReqCParam2,";")
                bNewCust.ZipCode    = ENTRY(2,MsRequest.ReqCParam2,";")
                bNewCust.PostOffice = ENTRY(3,MsRequest.ReqCParam2,";")
                bNewCust.Country    = ENTRY(4,MsRequest.ReqCParam2,";")
                bNewCust.EMail      = ENTRY(1,MsRequest.ReqCParam3,";")
                bNewCust.SMSNumber  = ENTRY(2,MsRequest.ReqCParam3,";")
                bNewCust.Category   = ENTRY(1,MsRequest.ReqCParam4,";")
                bNewCust.ContrBeg   = TODAY.

         FIND CustCat WHERE 
              CustCat.Brand    = gcBrand AND
              CustCat.Category = bNewCust.Category 
         NO-LOCK NO-ERROR.
         IF AVAILABLE CustCat THEN bNewCust.PaymTerm = CustCat.PaymTerm.
      END.

      ELSE liCreated = 0.

   END. 

   /* an existing customer has been chosen to be the new user 
      (and now the new customer has also been created) */
   IF liNewUser > 0 THEN DO:
   
      FIND bNewCust WHERE bNewCust.CustNum = liNewUser
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bNewCust THEN DO:
         fReqError("New user customer not found").
         RETURN.
      END. 

      IF bNewCust.AgrCust NE MobSub.AgrCust THEN DO:
         fReqError("Conflict in agreement customer definition of user").
         RETURN.
      END. 
   
      IF MsRequest.ReqType    = 3  AND
         MsRequest.ReqIParam2 > 0  AND
         bNewCust.InvCust NE liNewInvCust 
      THEN DO:
         fReqError("Conflict in invoicing customer definition of user").
         RETURN.
      END. 

      /* mark new invoice customer to user */
      IF MsRequest.ReqType = 4 AND
         bNewCust.InvCust NE liNewInvCust THEN DO:
         
         FIND Customer WHERE Customer.CustNum = bNewCust.CustNum EXCLUSIVE-LOCK.
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).
         Customer.InvCust = liNewInvCust.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
         RELEASE Customer.
      END.
         
   END. 
   
   ELSE DO:
      fReqError("New user has not been properly defined").
      RETURN.
   END. 

   /* an existing customer has been chosen to be the new invoice customer
      (and now the new customer has also been created) */
   IF liNewInvCust > 0 THEN DO:
   
      FIND bNewCust WHERE bNewCust.CustNum = liNewInvCust
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bNewCust THEN DO:
         fReqError("New invoice customer not found").
         RETURN.
      END. 

      IF bNewCust.AgrCust NE MobSub.AgrCust THEN DO:
         fReqError("Conflict in agreement customer definition of inv.customer").
         RETURN.
      END. 
   END. 
   
   ELSE DO:
      fReqError("New invoice customer has not been properly defined").
      RETURN.
   END. 

   /* move fees, create a new msowner etc. */
   RUN pMsCustMove (MobSub.AgrCust,
                    IF liNewInvCust > 0 THEN liNewInvCust ELSE liOldInvCust,
                    IF liNewUser > 0 THEN liNewUser ELSE liOldUser).

   /* new web id and password for new customer,
      make also sure that old customers have the account */
   DO liReqCnt = 1 TO 2:
   
      CASE liReqCnt:
      WHEN 1 THEN DO:
         IF liNewUser = liOldUser THEN NEXT.
         liUserCust = liNewUser.
      END.
      WHEN 2 THEN DO:
         IF liNewInvCust = liNewUser OR
            liNewInvCust = liOldInvCust
         THEN NEXT.  
         liUserCust = liNewInvCust.
      END.
      END CASE.

      IF liUserCust = 0 THEN NEXT. 
        
      /* has customer already used ids */
      FIND FIRST UserAccount WHERE UserAccount.CustNum = liUserCust 
         NO-LOCK NO-ERROR.
      IF AVAILABLE UserAccount AND UserAccount.Active = 2 THEN NEXT. 
      
      /* create a new account */
      create_account(liUserCust,?,?).

      /* print a letter */
      RUN prinuser(liUserCust,
                   "new", 
                   OUTPUT lcReqChar).

      IF lcReqChar > "" THEN DO:
         fReqLog("User account letter print failed: " + lcReqChar).
      END. 
   END.
   
   /* send SMS */
   IF MsRequest.SendSMS = 1 THEN DO:

      FIND bNewCust WHERE bNewCust.CustNum = liNewUser NO-LOCK.
      
      lcSMSText = fGetTxt("SMS",
                          IF MsRequest.ReqType = 3
                          THEN "UserChange"
                          ELSE "InvCustChange",
                          TODAY,
                          bNewCust.Language).

      IF lcSMSText > "" THEN DO:                    
         ASSIGN 
            lcSMSText = REPLACE(lcSMSText,"#NewUser",
                                          TRIM(STRING(liNewUser,
                                                      ">>>>>>9.99"))).
            lcSMSText = REPLACE(lcSMSText,"#NewInvCust",
                                          TRIM(STRING(liNewInvCust,
                                                      ">>>>>>9.99"))).
              
         /* replace tags */
         fReplaceSMS(lcSMSText,
                     MsRequest.MsSeq,
                     TODAY,
                     OUTPUT lcSMSText).


         /* don't send messages before 8 am. */
         ldReqStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
         IF ldReqStamp = ? THEN ldReqStamp = fMakeTS().

         fMakeSchedSMS(MobSub.CustNum,
                       MobSub.CLI,
                       15,
                       lcSMSText,
                       ldReqStamp).
      END. 
   END.
 
   RELEASE bNewCust.
   RELEASE MobSub.

   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.


/* agreement customer change for subscription */
PROCEDURE pOwnerChange:

   DEF VAR liDefCust    AS INT  NO-UNDO. 
   DEF VAR liNewOwner   AS INT  NO-UNDO.
   DEF VAR liNewInvCust AS INT  NO-UNDO.
   DEF VAR liNewUser    AS INT  NO-UNDO.
   DEF VAR liChkCust    AS INT  NO-UNDO.
   DEF VAR liCreated    AS INT  NO-UNDO EXTENT 3.
   DEF VAR lcCategory   AS CHAR NO-UNDO. 
   DEF VAR lcCustType   AS CHAR NO-UNDO.
   DEF VAR liOldOwner   AS INT  NO-UNDO.
   DEF VAR llNewCust    AS LOG  NO-UNDO. 

   DEF BUFFER bNewCust    FOR Customer.
       
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("MobSub not found").
      RETURN.
   END.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq NO-LOCK.
   IF NOT AVAILABLE MsOwner OR MsOwner.CustNum NE MobSub.CustNum OR
      MsOwner.AgrCust NE MobSub.AgrCust OR MsOwner.TSEnd < MsRequest.ActStamp
   THEN DO:
      fReqError("Invalid MSOwner data").
      RETURN.
   END. 
 
   /* nor an existing customer nbr or a name for the new customer is given */
   IF MsRequest.ReqIParam1 = 0 AND MsRequest.ReqCParam1 = "" THEN DO:
      fReqError("Nothing to do").
      RETURN. 
   END.

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
 
   ASSIGN liNewOwner   = MsRequest.ReqIParam1
          liOldOwner   = MobSub.AgrCust
          liCreated    = 0.

   /* a new customer will be created */
   DO liReqCnt = 1 TO 3:
   
      CASE liReqCnt:
      WHEN 1 THEN DO:
         /* if vrk has been run succesfully for old owner then update 
            old owner's data, otherwise skip this */
         IF liNewOwner > 0 AND MsRequest.ReqDParam1 NE 1 THEN NEXT.
         lcCategory = ENTRY(10,MsRequest.ReqCParam1,";").
      END.

      WHEN 2 THEN DO:
         /* is agrcust also invcust */
         CASE SUBSTRING(MsRequest.ReqCParam4,2,1):
         WHEN "1" THEN liNewInvCust = liNewOwner.
         WHEN "2" THEN DO:
            liNewInvCust = MsRequest.ReqIParam2.
            IF NUM-ENTRIES(MsRequest.ReqCParam2,";") < 10 THEN DO:
               fReqError("Invoice customer data missing").
               RETURN.
            END.
         END.
         OTHERWISE liNewInvCust = 0.
         END CASE.

         IF liNewInvCust > 0 THEN NEXT.

         lcCategory = ENTRY(10,MsRequest.ReqCParam2,";").
      END.
         
      WHEN 3 THEN DO:
       
         /* is agrcust or invcust also the user */
         CASE SUBSTRING(MsRequest.ReqCParam4,3,1):
         WHEN "1" THEN liNewUser = liNewOwner.
         WHEN "2" THEN liNewUser = liNewInvCust.
         WHEN "3" THEN DO:
            liNewUser = INTEGER(ENTRY(11,MsRequest.ReqCParam3,";")).
            IF NUM-ENTRIES(MsRequest.ReqCParam3,";") < 10 THEN DO:
               fReqError("User data missing").
               RETURN.
            END.
         END. 
         OTHERWISE liNewUser = 0.
         END CASE.

         IF liNewUser > 0 THEN DO:
            /* if agrcust = user and it is a new customer number, but invcust 
               is a different one -> mark correct invcust to new owner/user */
            IF liNewUser = liNewOwner AND MsRequest.ReqIParam1 = 0 AND
               liNewInvCust NE liNewOwner THEN DO:
               
               FIND bNewCust WHERE bNewCust.CustNum = liNewInvCust 
                  NO-LOCK NO-ERROR.
               IF AVAILABLE bNewCust AND bNewCust.AgrCust = liNewOwner THEN DO:
                  FIND bNewCust WHERE bNewCust.CustNum = liNewUser 
                     EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE bNewCust THEN bNewCust.InvCust = liNewInvCust.
               END.   
            END.
               
            NEXT.
         END.
         
         lcCategory = ENTRY(10,MsRequest.ReqCParam3,";").
      END.
         
      END CASE. 

      /* create a new customer */
      IF (liReqCnt = 1 AND liNewOwner = 0) OR liReqCnt > 1 THEN DO:
      
         /* group from invoicing customer */
         IF liNewInvCust > 0 
         THEN FIND Customer WHERE Customer.CustNum = liNewInvCust 
                 NO-LOCK NO-ERROR.
         /* use current one's inv.group if a new inv.cust is created */  
         ELSE FIND Customer WHERE Customer.CustNum = MobSub.InvCust 
                 NO-LOCK NO-ERROR.
      
         IF NOT AVAILABLE Customer THEN DO:
            fReqError("Unknown invoicing customer").
            RETURN.
         END. 
         
         liDefCust = fCParamI("DefCust" + Customer.InvGroup + "/" +
                              lcCategory).
      
         IF liDefCust = ? OR liDefCust = 0 THEN DO:
            fReqError("Default customer not defined for " + Customer.InvGroup).
            RETURN.
         END. 
      
         ASSIGN liCreated[liReqCnt] = liDefCust
                llNewCust           = TRUE.

         RUN copymobcu (INPUT-OUTPUT liCreated[liReqCnt],
                        FALSE).
      END.
      
      /* update old customer's data if vrk has been succesful */
      ELSE IF liReqCnt = 1 AND liNewOwner > 0 THEN ASSIGN 
         liCreated[1] = liNewOwner
         llNewCust    = FALSE.
      
      FIND bNewCust WHERE bNewCust.CustNum = liCreated[liReqCnt] 
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bNewCust THEN DO:
      
         CASE liReqCnt:
         WHEN 1 THEN DO: 
            ASSIGN 
               liNewOwner          = bNewCust.CustNum
               bNewCust.InvCust    = bNewCust.CustNum WHEN llNewCust
               bNewCust.CustName   = ENTRY(1,MsRequest.ReqCParam1,";")
               bNewCust.FirstName  = ENTRY(2,MsRequest.ReqCParam1,";")
               bNewCust.COName     = ENTRY(3,MsRequest.ReqCParam1,";")
               bNewCust.Address    = ENTRY(4,MsRequest.ReqCParam1,";")
               bNewCust.ZipCode    = ENTRY(5,MsRequest.ReqCParam1,";")
               bNewCust.PostOffice = ENTRY(6,MsRequest.ReqCParam1,";")
               bNewCust.Country    = ENTRY(7,MsRequest.ReqCParam1,";")
               bNewCust.OrgID      = ENTRY(11,MsRequest.ReqCParam1,";").

            /* update these only if values have been given (in case an
               old customer has been chosen) */
            IF ENTRY(8,MsRequest.ReqCParam1,";") > "" THEN 
               bNewCust.EMail      = ENTRY(8,MsRequest.ReqCParam1,";").
            IF ENTRY(9,MsRequest.ReqCParam1,";") > "" THEN 
               bNewCust.SMSNumber  = ENTRY(9,MsRequest.ReqCParam1,";").
         END.
         
         WHEN 2 THEN ASSIGN 
            liNewInvCust        = bNewCust.CustNum
            bNewCust.InvCust    = bNewCust.CustNum
            bNewCust.CustName   = ENTRY(1,MsRequest.ReqCParam2,";")
            bNewCust.FirstName  = ENTRY(2,MsRequest.ReqCParam2,";")
            bNewCust.COName     = ENTRY(3,MsRequest.ReqCParam2,";")
            bNewCust.Address    = ENTRY(4,MsRequest.ReqCParam2,";")
            bNewCust.ZipCode    = ENTRY(5,MsRequest.ReqCParam2,";")
            bNewCust.PostOffice = ENTRY(6,MsRequest.ReqCParam2,";")
            bNewCust.Country    = ENTRY(7,MsRequest.ReqCParam2,";")
            bNewCust.EMail      = ENTRY(8,MsRequest.ReqCParam2,";")
            bNewCust.SMSNumber  = ENTRY(9,MsRequest.ReqCParam2,";").

         WHEN 3 THEN ASSIGN 
            liNewUser           = bNewCust.CustNum
            bNewCust.InvCust    = liNewInvCust
            bNewCust.CustName   = ENTRY(1,MsRequest.ReqCParam3,";")
            bNewCust.FirstName  = ENTRY(2,MsRequest.ReqCParam3,";")
            bNewCust.COName     = ENTRY(3,MsRequest.ReqCParam3,";")
            bNewCust.Address    = ENTRY(4,MsRequest.ReqCParam3,";")
            bNewCust.ZipCode    = ENTRY(5,MsRequest.ReqCParam3,";")
            bNewCust.PostOffice = ENTRY(6,MsRequest.ReqCParam3,";")
            bNewCust.Country    = ENTRY(7,MsRequest.ReqCParam3,";")
            bNewCust.EMail      = ENTRY(8,MsRequest.ReqCParam3,";")
            bNewCust.SMSNumber  = ENTRY(9,MsRequest.ReqCParam3,";").

         END CASE.
         
         IF llNewCust THEN DO:
            ASSIGN 
            bNewCust.ChgStamp   = fMakeTS()
            bNewCust.CreUser    = katun
            bNewCust.PaymCust   = liNewOwner
            bNewCust.AgrCust    = liNewOwner
            bNewCust.RepCust    = bNewCust.CustNum
            bNewCust.RateCust   = bNewCust.CustNum 
            bNewCust.SearchName = SUBSTRING(bNewCust.CustName + " " + 
                                            bNewCust.FirstName,1,8)
            bNewCust.Category   = lcCategory
            bNewCust.ContrBeg   = TODAY.

            FIND CustCat WHERE 
                 CustCat.Brand    = gcBrand AND
                 CustCat.Category = bNewCust.Category NO-LOCK NO-ERROR.
            IF AVAILABLE CustCat THEN bNewCust.PaymTerm = CustCat.PaymTerm.
         END.   
      END.

      ELSE liCreated[liReqCnt] = 0.
   END. 

   /* an existing customer has been chosen to be the new one 
      (and now the new customers have also been created) */
   DO liReqCnt = 1 TO 3:
       
      CASE liReqCnt:
      WHEN 1 THEN ASSIGN liChkCust  = liNewOwner
                         lcCustType = "agr.customer".
      WHEN 2 THEN ASSIGN liChkCust  = liNewInvCust
                         lcCustType = "inv.customer".
      WHEN 3 THEN ASSIGN liChkCust  = liNewUser
                         lcCustType = "user".
      END CASE.
      
      IF liChkCust = 0 THEN DO:
         fReqError("Customer not defined for " + lcCustType).
         RETURN.
      END. 
   
      FIND bNewCust WHERE bNewCust.CustNum = liChkCust
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bNewCust THEN DO:
         fReqError("New " + lcCustType + " customer not found").
         RETURN.
      END. 

      IF bNewCust.AgrCust NE liNewOwner THEN DO:
         fReqError("Conflict in agreement customer definition of " +
                   lcCustType).
         RETURN.
      END. 
   
      IF liReqCnt >= 2 AND bNewCust.InvCust NE liNewInvCust 
      THEN DO:
         fReqError("Conflict in invoicing customer definition of " +
                   lcCustType).
         RETURN.
      END. 
   END. 
 
   /* move fees, create a new msowner etc. */
   RUN pMsCustMove (liNewOwner,
                    liNewInvCust,
                    liNewUser).

   /* new web id and password for new customer,
      make also sure that old customers have the account */
   DO liReqCnt = 1 TO 3:
   
      IF liReqCnt = 2 AND liNewInvCust = liNewOwner THEN NEXT.
      IF liReqCnt = 3 AND (liNewUser = liNewInvCust OR liNewUser = liNewOwner)
      THEN NEXT.  
      
      CASE liReqCnt:
      WHEN 1 THEN liChkCust = liNewOwner.
      WHEN 2 THEN liChkCust = liNewInvCust.
      WHEN 3 THEN liChkCust = liNewUser.
      END CASE.

      /* has customer already used ids */
      FIND FIRST UserAccount WHERE UserAccount.CustNum = liChkCust 
         NO-LOCK NO-ERROR.
      IF AVAILABLE UserAccount AND UserAccount.Active = 2 THEN NEXT. 
 
      /* create a new account */
      create_account(liChkCust,?,?).

      /* print a letter */
      IF liReqCnt >= 2 THEN DO:
         RUN prinuser(liChkCust,
                      "ownerchg", 
                      OUTPUT lcReqChar).

         IF lcReqChar > "" THEN DO:
            fReqLog("User account letter print failed (ownerchg): " +
                    lcReqChar).
         END. 
      END.
   END.

   /* confirmation letters to both old and new owner */
   RUN prinocconf(liOldOwner,
                  liNewOwner,
                  MsRequest.MsSeq,
                  MsRequest.CLI,
                  OUTPUT lcReqChar).
                  
   IF lcReqChar > "" THEN DO:
     fReqLog("Owner change confirmation letter print failed: " + lcReqChar).
   END. 
 
   /* send SMS */
   IF MsRequest.SendSMS = 1 THEN DO:

      FIND bNewCust WHERE bNewCust.CustNum = liNewOwner NO-LOCK.
      
      lcSMSText = fGetTxt("SMS",
                          "OwnerChange",
                          TODAY,
                          bNewCust.Language).

      IF lcSMSText > "" THEN DO:                    
         ASSIGN 
            lcSMSText = REPLACE(lcSMSText,"#NewUser",
                                          TRIM(STRING(liNewUser,
                                                      ">>>>>>9.99")))
            lcSMSText = REPLACE(lcSMSText,"#NewInvCust",
                                          TRIM(STRING(liNewInvCust,
                                                      ">>>>>>9.99")))
            lcSMSText = REPLACE(lcSMSText,"#NewOwner",
                                          TRIM(STRING(liNewOwner,
                                                      ">>>>>>9.99"))).
              
         /* replace tags */
         fReplaceSMS(lcSMSText,
                     MsRequest.MsSeq,
                     TODAY,
                     OUTPUT lcSMSText).


         /* don't send messages before 8 am. */
         ldReqStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
         IF ldReqStamp = ? THEN ldReqStamp = fMakeTS().

         fMakeSchedSMS(liNewOwner,
                       bNewCust.SMSNumber,
                       20,
                       lcSMSText,
                       ldReqStamp).
      END. 
   END.

   /* fee from owner change to new customer (actually to invoice customer) */
   IF MsRequest.CreateFees THEN DO:
   
      RUN creasfee (0,
                    MsRequest.MsSeq,
                    TODAY,
                    "MobSub",
                    "CHANGEOWNER",
                    1,
                    ?,
                    /* memo   */
                    STRING(liOldOwner) + " -> " + STRING(liNewOwner),
                    FALSE,          /* no messages to screen */
                    katun,
                    "",
                    0,
                    "",
                    "",
                    OUTPUT lcReqChar).
   END.
   
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.

/* move fees to new customer etc. */
PROCEDURE pMsCustMove:

   DEF INPUT PARAMETER iiNewOwner   AS INT NO-UNDO. 
   DEF INPUT PARAMETER iiNewInvCust AS INT NO-UNDO. 
   DEF INPUT PARAMETER iiNewUser    AS INT NO-UNDO. 
   
   DEF VAR liNewTarget  AS INT  NO-UNDO.
   DEF VAR liFeePeriod  AS INT  NO-UNDO. 
   DEF VAR ldtFeeFrom   AS DATE NO-UNDO.
   DEF VAR ldtFeeTo     AS DATE NO-UNDO. 
   DEF VAR ldFeeAmt     AS DEC  NO-UNDO. 
   DEF VAR liPerDays    AS INT  NO-UNDO.
   DEF VAR ldtFeeDate   AS DATE NO-UNDO.
   DEF VAR ldEndStamp   AS DEC  NO-UNDO.

   DEF BUFFER bBillTarget FOR BillTarget.
   DEF BUFFER bOwner      FOR MSOwner.
   DEF BUFFER bOMobSub    FOR MobSub.
   DEF BUFFER bFixedFee   FOR FixedFee.
   DEF BUFFER bFFItem     FOR FFItem.
   DEF BUFFER bFatime     FOR Fatime.
   
   /* user has been changed -> mark new user nbr to related tables */
   IF MobSub.CustNum NE iiNewUser AND iiNewUser > 0 THEN DO:
      /* make sure that new user has a similar billing target */
      FIND bBillTarget WHERE
           bBillTarget.CustNum    = MobSub.CustNum AND
           bBillTarget.BillTarget = MobSub.BillTarget NO-LOCK NO-ERROR.
         
      FIND BillTarget WHERE
           BillTarget.CustNum    = iiNewUser AND
           BillTarget.BillTarget = MobSub.BillTarget NO-LOCK NO-ERROR.
   
      liNewTarget = 0.

      IF AVAILABLE BillTarget AND AVAILABLE bBillTarget THEN DO: 

         IF BillTarget.RatePlan NE bBillTarget.RatePlan OR
            BillTarget.DiscPlan NE bBillTarget.DiscPlan
         THEN DO liReqCnt = 30 TO 99:
            IF NOT CAN-FIND(BillTarget WHERE
                            BillTarget.CustNum    = iiNewUser AND
                            BillTarget.BillTarget = liReqCnt)
            THEN DO:
               liNewTarget = liReqCnt.
              LEAVE.
            END.
         END.   
      END.

      IF NOT AVAILABLE BillTarget OR liNewTarget > 0 THEN DO:
      
         CREATE BillTarget.
      
         IF AVAILABLE bBillTarget THEN DO:
            BUFFER-COPY bBillTarget EXCEPT CustNum TO BillTarget.
            IF liNewTarget > 0 THEN BillTarget.BillTarget = liNewTarget.
         END.

         ELSE DO:
        
            FIND CLIType WHERE 
                 CLIType.Brand   = gcBrand AND
                 CLIType.CLIType = MobSub.CLIType NO-LOCK NO-ERROR.
            IF AVAILABLE CLIType THEN ASSIGN 
               BillTarget.BillTarget = CLIType.BillTarget
               BillTarget.RatePlan   = CLIType.PricePlan
               BillTarget.DiscPlan   = CLIType.DiscPlan.
            ELSE ASSIGN 
               BillTarget.BillTarget = 1.
         END.
      
         BillTarget.CustNum = iiNewUser.
      END.

      /* change period */
      ASSIGN liFeePeriod = YEAR(ldtActDate) * 100 + MONTH(ldtActDate)
             ldtFeeFrom  = DATE(MONTH(ldtActDate),1,YEAR(ldtActDate)).
      IF MONTH(ldtActDate) = 12
      THEN ldtFeeTo = DATE(12,31,YEAR(ldtActDate)).
      ELSE ldtFeeTo = DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)) - 1.
    
      FOR EACH FATime EXCLUSIVE-LOCK USE-INDEX MobSub WHERE
               FATime.Brand  = gcBrand      AND
               FATime.MsSeq  = MobSub.MsSeq AND
               FATime.InvNum = 0            AND
               FATime.Period >= liFeePeriod:
      
         IF FATime.CustNum = iiNewUser THEN NEXT.
         
         /* split fatimes for change month to both customers */
         IF Fatime.Period = liFeePeriod THEN DO:
      
            CREATE bFatime.
            BUFFER-COPY Fatime EXCEPT FatNum FatID TO bFatime.
            ASSIGN 
               bFatime.FatNum   = NEXT-VALUE(FTSeq)
               bFatime.FatID    = NEXT-VALUE(FT-Seq)
               bFatime.CustNum  = iiNewUser
               ldFeeAmt         = (Fatime.Amt - Fatime.Used - Fatime.TransQty) 
                                  / (ldtFeeTo - ldtFeeFrom + 1)
               ldFeeAmt         = ldFeeAmt * (ldtActDate - ldtFeeFrom + 1)
               Fatime.Amt       = ldFeeAmt
               bFatime.Amt      = bFatime.Amt - Fatime.Amt
               bFatime.Used     = 0 
               bFatime.TransQty = 0.

            /* memo */
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "FATime",
                       STRING(bFatime.FatNum),
                       bFatime.CustNum,
                       "User Change",
                       "Transferred from customer " + STRING(Fatime.CustNum)).
         END.

         /* transfer newer fatimes totally */
         ELSE DO:
            /* memo */
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "FATime",
                       STRING(Fatime.FatNum),
                       iiNewUser,
                       "User Change",
                       "Transferred from customer " + STRING(Fatime.CustNum)).

            FATime.CustNum = iiNewUser. 
         END.   
      END.
   
      /* MSISDN  */
      FIND FIRST MSISDN EXCLUSIVE-LOCK WHERE 
                 MSISDN.CLI = MobSub.CLI NO-ERROR.
      IF AVAILABLE MSISDN THEN MSISDN.CustNum = iiNewUser.
   
      /* IMSI */
      FIND FIRST IMSI EXCLUSIVE-LOCK WHERE
                 IMSI.ICC = MobSub.ICC NO-ERROR.
      IF AVAILABLE IMSI THEN IMSI.CustNum = iiNewUser.
             
      /* SIM */
      FIND FIRST SIM EXCLUSIVE-LOCK WHERE
                 SIM.Brand = gcBrand   AND
                 SIM.ICC   = MobSub.ICC NO-ERROR.
      IF AVAILABLE SIM THEN SIM.CustNum = iiNewUser.
   END.

   /* invoicing customer is changed -> move fees */
   IF MobSub.InvCust NE iiNewInvCust AND iiNewInvCust > 0 THEN DO:

      /* fees belong to old customer till the end of change month,
         unless change happens on 1. day before 3 am */
      ldtFeeDate = ldtActDate.
  
      IF DAY(ldtFeeDate) > 1 OR liActTime > 10800 THEN DO:
         IF MONTH(ldtFeeDate) = 12
         THEN ldtFeeDate = DATE(12,31,YEAR(ldtFeeDate)).
         ELSE ldtFeeDate = DATE(MONTH(ldtFeeDate) + 1,1,YEAR(ldtFeeDate)) - 1. 
      END.

      liFeePeriod = YEAR(ldtFeeDate) * 10000 + 
                    MONTH(ldtFeeDate) * 100  + 
                    DAY(ldtFeeDate).
                    
      FOR EACH FixedFee EXCLUSIVE-LOCK WHERE
               FixedFee.Brand     = gcBrand              AND
               FixedFee.HostTable = "MobSub"             AND 
               FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
               FixedFee.InUse     = TRUE:
         
         IF NOT CAN-FIND(FIRST FFItem OF FixedFee WHERE
                               FFItem.Billed = FALSE AND
                               FFItem.Concerns[2] > liFeePeriod)
         THEN NEXT.
         
         /* shouldn't be, but better to check */   
         IF FixedFee.CustNum = iiNewInvCust THEN NEXT. 
         
         CREATE bFixedFee.
         BUFFER-COPY FixedFee EXCEPT CustNum FFNum TO bFixedFee.
         
         ASSIGN bFixedFee.FFNum   = NEXT-VALUE(Contract)
                bFixedFee.CustNum = iiNewInvCust
                bFixedFee.BegDate = ldtFeeDate. 
                
         FOR EACH FFItem OF FixedFee EXCLUSIVE-LOCK WHERE
                  FFItem.Billed      = FALSE AND
                  FFItem.Concerns[2] > liFeePeriod:
              
            /* split for both customers */
            IF FFItem.Concerns[1] < liFeePeriod THEN DO:
            
               CREATE bFFItem.
               BUFFER-COPY FFItem EXCEPT FFNum FFItemNum TO bFFItem.
               ASSIGN bFFItem.FFItemNum   = NEXT-VALUE(Citem)
                      bFFItem.FFNum       = bFixedFee.FFNum
                      bFFItem.CustNum     = bFixedFee.CustNum
                      bFFItem.Concerns[1] = liFeePeriod.
                      
               ldtFeeFrom = fInt2Date(FFItem.Concerns[1],1).
               ldtFeeTo   = fInt2Date(FFItem.Concerns[2],2).
               
               ASSIGN ldFeeAmt            = FFItem.Amt / 
                                            (ldtFeeFrom - ldtFeeTo + 1)
                      ldFeeAmt            = ldFeeAmt * 
                                            (ldtFeeDate - ldtFeeFrom)
                      FFItem.Amt          = ldFeeAmt
                      FFItem.Concerns[2]  = YEAR(ldtFeeDate - 1) * 10000 + 
                                            MONTH(ldtFeeDate - 1) * 100  +
                                            DAY(ldtFeeDate - 1)
                      bFFItem.Amt         = bFFItem.Amt - FFItem.Amt.
                      
            END. 

            ELSE ASSIGN FFItem.FFNum   = bFixedFee.FFNum
                        FFItem.CustNum = bFixedFee.CustNum.
         END. 

         FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
         IF NOT AVAILABLE FFItem 
         THEN ASSIGN FixedFee.EndPer = FixedFee.BegPer
                     FixedFee.InUse  = FALSE.
         ELSE FixedFee.EndPer = FFItem.BillPeriod.
         
         FIND FIRST bFFItem OF bFixedFee NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bFFItem
         THEN bFixedFee.InUse = FALSE.
         ELSE bFixedFee.BegPer = bFFItem.BillPer.

         /* memo */
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "FixedFee",
                    STRING(bFixedFee.FFNum),
                    bFixedFee.CustNum,
                    "User Change",
                    "Transferred from customer " + STRING(FixedFee.CustNum)).
          
      END. 

      liFeePeriod = TRUNCATE(liFeePeriod / 100,0).
      
   END.

   /* end current msowner and create a new one */
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq AND
              MsOwner.TSEnd >= 99999999
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq EXCLUSIVE-LOCK.
   ASSIGN ldEndStamp    = MsOwner.TSEnd
          MsOwner.TSEnd = MsRequest.ActStamp - 0.00001.
          
   CREATE bOwner.
   BUFFER-COPY MsOwner EXCEPT CustNum TSBeg TSEnd TO bOwner.
   ASSIGN bOwner.CustNum = iiNewUser    WHEN iiNewUser > 0
          bOwner.TSBeg   = MsRequest.ActStamp
          bOwner.TSEnd   = ldEndStamp
          bOwner.InvCust = iiNewInvCust WHEN iiNewInvCust > 0
          bOwner.AgrCust = iiNewOwner.

   RELEASE MsOwner.
   RELEASE bOwner.       

   /* new user to mobsub */
   FIND CURRENT MobSub EXCLUSIVE-LOCK.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobSub).
   ASSIGN MobSub.CustNum = iiNewUser    WHEN iiNewUser > 0
          MobSub.InvCust = iiNewInvCust WHEN iiNewInvCust > 0
          MobSub.AgrCust = iiNewOwner.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobSub).

   RELEASE MobSub.

   /* if invoice customer changed -> check other subscriptions of this same
      user */
   FOR EACH bOMobSub NO-LOCK WHERE
            bOMobSub.CustNum = iiNewUser AND
            bOMobSub.AgrCust = iiNewOwner:

      IF bOMobSub.InvCust NE iiNewInvCust THEN DO:
      
         FIND FIRST MsOwner WHERE
                    MsOwner.MsSeq = bOMobSub.MsSeq AND
                    MsOwner.TSEnd >= 99999999
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE MsOwner THEN 
         FIND FIRST MsOwner WHERE
                    MsOwner.MsSeq = bOMobSub.MsSeq EXCLUSIVE-LOCK.
         ASSIGN ldEndStamp    = MsOwner.TSEnd
                MsOwner.TSEnd = MsRequest.ActStamp - 0.00001.
          
         CREATE bOwner.
         BUFFER-COPY MsOwner EXCEPT CustNum TSBeg TSEnd TO bOwner.
         ASSIGN bOwner.CustNum = iiNewUser
                bOwner.TSBeg   = MsRequest.ActStamp
                bOwner.TSEnd   = ldEndStamp
                bOwner.InvCust = iiNewInvCust
                bOwner.AgrCust = iiNewOwner.

         RELEASE MsOwner.
         RELEASE bOwner.       

         /* new user to mobsub */
         FIND MobSub WHERE RECID(MobSub) = RECID(bOMobSub) EXCLUSIVE-LOCK.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobSub).
         MobSub.InvCust = iiNewInvCust.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobSub).
      END.     
   END.       
  
END PROCEDURE.



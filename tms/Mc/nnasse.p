/* ----------------------------------------------------------------------------
  MODULE .......: NNASSE.P
  FUNCTION .....: Customer Browse
  APPLICATION ..: NN
  CREATED ......: 21-01-96 PT
  CHANGED ......:
                  14.09.99 pt NEW fields Customer.BankAcct, Customer.DDStatus
                  22.09.99 kl check FOR DUMMY customer TO copy values
                  25.10.99 kl functions in ASSIGN
                  02.11.99 kl pCopy, more fields TO copy
                  02.12.99 kl RUN Help/custser.p WITH NEW customers
                  03.12.99 kl previous tuned
                  28.12.99 kl copying Friends & Family numbers from DUMMY
                  07.01.00 kl copy VATAmt code from DUMMY
                  08.02.00 kl sequence PriceId
                  24.02.00 kl TMSParam default values
                  24.03.00 ht memochr shown in FRAME sel
                  17.04.00 jp preselect
                  04.05.00 kl WEB invoice password deleting after F6
                  02.06.00 ht checking Salesman omitted (.i)
                  10.08.00 pt expanded formats: Balance Deposit @ FRAME fina
                  11.05.01 kl Customer.DirDisc in FRAME fina
                  10.01.02/aam Bank is NOT mandatory (financial data)
                  25.01.02/aam OrgId, Salesman AND Size NOT mandatory
                  14.02.02 jp button: F2 - mobile info
                  14.02.02 jp button: F3 - Fixed info
                  14.02.02 jp Renamed button F7: Customer info
                  05.03.02 lp Customer.AdvPaym in FRAME fina
                  14.05.02 jp copycu
                  14.05.02 tk Event logging added
                  17.05.02/tk RUN Mc/memo.p
                  28.05.02 kl dpaym removed
                  22.07.02 tk show full page on "end"
                  06.07.02/tk PListConf
                  07.08.02/jp display brand name
                  26.08.02/jp bank.Bankid is string field
                  26.08.02/jp CustClass
                  26.08.02/jr frame lis hide 
                  09.09.02/aam DirDiscPerc and VolDisc removed,
                               CustDiscProd removed,
                               fChkDiscount(),
                               delete BillTarg and RatePlan when cust deleted
                  16.09.02/aam PriceList and RateCust removed              
                  25.09.02/aam eventlog also for financial data,
                               customer balances in table CustBal and CustCount
                  01.10.02/aam VatIncl added (financial data)
                  09.10.02/jr  Country validate /name display
                  09.10.02/jr  Currency validate
                  09.10.02/jr  Uses number series in copy also
                  18.10.02/aam InvCode added 
                  31.10.02 lp  - eventlog also for MthCall
                               - added second parameter for nnasli
                  01.11.02 lp  - corrected memochr
                               - eventlog also for Invoice and 
                                 CGmember(after change)
                  13.11.02 jr  Removed N column (tariff.custnum)
                  13.11.02 jr  Removed C column (mthcall)               
                  20.11.02 lp  Customer.AgrCust added
                  27.01.03 aam DelType and ChargeType added to fin.data
                  30.01.03/jp  create ext.cust group record if Credit limit gt 
                               0, remove possible ext.cust group if limit 0
                  18.02.03 kl  PNP copying commented
                  26.02.03/jp  AddCustlimit - PRESS F9 for as-limi[2],
                               Open balance
                  28.02.03 aam ClaimPerm and InterestPerm added (financial),
                               DirMark and CCLang added,
                               get bankaccount from DD Authorization (nnsvte),
                               eMail-fields in frame femails 
                  05.03.03 aam extra tables removed from eventlog handling
                  07.03.03 aam balance[2] -> CreditLimit
                  10.03.03 tk  tokens finished
                  20.03.03 tk  custfind.i for full/reseller/brand browsing
                  23.03.03 aam default customers for each invgroup,
                               quicker find for first free number,
                               financial data before bill.targets in add mode
                  25.03.03 aam RateCust into use again      
                  28.03.03 aam column "P" for customer's net prices 
                  11.04.03 tk  show language and cclang names in frame lis
                  15.05.03 tk  show M for empty memo records also
                  22.05.03/aam EPL-file optionally when credit limit changed,
                               show belonging to credit limit group and
                               open cl-balance for cl-customers
                  28.05.03/aam open CLIs when credit limit removed,
                               show extra reference nbr etc. 
                  03.09.03 aam brand 
                  29.09.03 aam ChargeType, DelType
                  01.10.03 aam Salesman and Reseller removed,
                               SpecDel added
                  02.10.03 jp  f2 - new mobile info             
                  07.10.03 aam PaymTerm default from CustCat
                  09.10.03 jp  fina - view
                  20.11.03 jp  brand code when search invtarg
                  27.11.03 aam skip billtargets and clis when creating custs
                  03.12.03/aam VATUsage, VATIncl, IDel* added,
                  17.12.03 jp  email format x(76)
                  16.01.04/aam AgrCust added, AccGrp removed
                  22.01.04/jp  f7 - find mobile #
                  29.01.04/aam generate fee from RepCodes -change
                  06.02.04 jp  input parameter custnum for memo
                  06.02.04/aam brand was missing from custcat-find
                  09.02.04 jp  F2 - mobile info again
                  09.02.04/aam show stars (*) if delivery address exists,
                               set AgrCust when template used
                  11.02.04/jp  f3 - show users , consulted by Teemu            
                  16.02.04 jp  modify custfind 
                  27.02.04 tk  f4 - email address removed
                  15.03.04/aam limit size of transactions
                  29.03.04/aam update BankAcc if no dd-authorization
                  08.04.04 aam new parameters for creasfee
                  27.04.04/aam show old values after cancel in change 
                  31.05.04/aam SMSNumber
                  02.08.04/aam show both invoice and adv.payment reference
                  19.10.04/aam ufkeys for local-update-fin
                  25.04.05/aam no find-functions when liMainCust > 0
                  02.11.05/tk  leave add-new if custser not selected
                  07.12.05/aam new layout, commontt from change screen etc. 
                  16.12.05 jp  iiCustnum
                  16.05.06/aam ask passwd for changing default nbr on add
                  12.06.06/aam transaction removed before running commontt
                  27.06.06/aam EndInvType and EndInvDate
                  14.11.06/aam yoigo layout and fields
                  24.01.07 kl  NIE into ID/Country check
                  29.01.07 kl  search-2 modified a lot
                  18.04.07/aam CreditLimit, used for od invoices
                  25.10.07/vk  F5-add new and F6-delete buttons deactivated
                  11.04.17/CS  Modify for customer history dump.

  Version ......: M15
  --------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER iiCustNum AS INT  NO-UNDO.
DEFINE INPUT PARAMETER icType    AS CHAR NO-UNDO.

&GLOBAL-DEFINE BrTable Customer

{Syst/commali.i}
{Func/lib/accesslog.i}

{Func/cparam2.i}
{Syst/eventval.i}
{Func/fcustbal.i}
{Func/fcustcnt.i}
{Func/fctype.i}
{Func/refcode.i}
{Func/fcustref.i}
{Func/frefnum.i}
{Func/fbankdata.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'customer'}
{Func/fcustdata.i}
{Func/matrix.i}
{Func/fmakemsreq.i}
{Func/flimitreq.i}
{Func/femailinvoice.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCustomer).
   END.

END.


DEF NEW shared VAR siirto AS CHAR.
DEF BUFFER xCustomer  FOR Customer.
DEF BUFFER BaseCust   FOR Customer.
DEF BUFFER BaseTarget FOR BillTarget.

IF llDoevent THEN DO: 

   DEFINE VARIABLE lhCLISer   AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhCLI      AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhWInvPwd  AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhCGMember AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhBillTarg AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhRatePlan AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhMthCall  AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhInvoice  AS HANDLE NO-UNDO.

   lhCLISer        = BUFFER CLISer:HANDLE.
   lhCLI           = BUFFER CLI:HANDLE.
   lhWInvPwd       = BUFFER WInvPwd:HANDLE.
   lhCGMember      = BUFFER CGMember:HANDLE.
   lhBillTarg      = BUFFER BillTarg:HANDLE.
   lhRatePlan      = BUFFER RatePlan:HANDLE.
   lhMthCall       = BUFFER MthCall:HANDLE.
   lhInvoice       = BUFFER Invoice:HANDLE.

   RUN StarEventInitialize(lhCLISer).
   RUN StarEventInitialize(lhCLI).
   RUN StarEventInitialize(lhWInvPwd).
   RUN StarEventInitialize(lhCGMember).
   RUN StarEventInitialize(lhBillTarg).
   RUN StarEventInitialize(lhRatePlan).
   RUN StarEventInitialize(lhMthCall).
   RUN StarEventInitialize(lhInvoice).

END.


DEF VAR CustNum     LIKE Customer.CustNum    NO-UNDO.
DEF VAR CustName    LIKE Customer.CustName   NO-UNDO.
DEF VAR SearchName  LIKE Customer.SearchName NO-UNDO.
DEF VAR OrgId       LIKE Customer.OrgId      NO-UNDO.
DEF VAR lcZip       LIKE Customer.ZipCode    NO-UNDO. 

DEF VAR lcFirstName  AS CHAR            NO-UNDO. 
DEF VAR pre          AS c               NO-UNDO.
DEF VAR rc           AS INT             NO-UNDO.
DEF VAR firstline    AS INT             NO-UNDO.
DEF VAR order        AS INT             NO-UNDO.
DEF VAR ex-order     AS INT             NO-UNDO.
DEF VAR memory       AS RECID           NO-UNDO.
def var line         as int format "99" NO-UNDO.
DEF VAR delline      AS INT             NO-UNDO.
DEF VAR must-print   AS LOG             NO-UNDO.
DEF VAR must-add     AS LOG             NO-UNDO.
DEF VAR ufkey        AS LOG             NO-UNDO.
DEF VAR fr-header    AS CHAR            NO-UNDO.
def var ok           as log format "Yes/No"     NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24         NO-UNDO.
DEF VAR i            AS INT                     NO-UNDO.
DEF VAR lcCountry    AS CHAR FORMAT "x(20)"     NO-UNDO.
def var igname       as char format "x(12)"     NO-UNDO.
DEF VAR new_custNo   AS INT                     NO-UNDO.
def var dkatnimi     as char format "x(10)"     NO-UNDO.
DEF VAR xrecid       AS RECID                   NO-UNDO.
def var Month        as i   format "999999"     NO-UNDO.
DEF VAR d            AS lo  format "*/"         NO-UNDO LABEL "D".
def var mess         as c   format "x(34)"      NO-UNDO EXTENT 5.
DEF VAR x1           AS i   NO-UNDO.
DEF VAR x2           AS i   NO-UNDO.
def var memochr      AS LOG NO-UNDO.
DEF VAR CLI          LIKE CLI.CLI NO-UNDO.
DEF VAR debug        AS LOG    NO-UNDO INIT TRUE.
DEF VAR save-ehto    AS INTEGER NO-UNDO.
DEF VAR save-ufk     AS INTEGER EXTENT 9 NO-UNDO.
DEF VAR lcBankAcc    AS CHAR   NO-UNDO.
DEF VAR llDDBank     AS LOG    NO-UNDO. 
DEF VAR lcTyyppi     AS CHAR   NO-UNDO.
DEF VAR liDefCust    AS INT    NO-UNDO. 
DEF VAR lcInvGroup   AS CHAR   NO-UNDO. 
DEF VAR llCustPrice  AS LOG    NO-UNDO. 
DEF VAR lcLang       AS CHAR   NO-UNDO FORMAT "x(20)".
DEF VAR lcCCLang     AS CHAR   NO-UNDO.

DEF VAR lcDType       AS CHAR  NO-UNDO.
DEF VAR lcSType       AS CHAR  NO-UNDO.
DEF VAR lcCType       AS CHAR  NO-UNDO.
DEF VAR lcCode        AS CHAR  NO-UNDO. 
DEF VAR lcFile        AS CHAR  NO-UNDO. 

DEF VAR llCLGroup     AS LOG   NO-UNDO. 
DEF VAR llOldCL       AS LOG   NO-UNDO. 
DEF VAR ldCLBal       AS DEC   NO-UNDO.
DEF VAR ldCLAlarm     AS DEC   NO-UNDO. 
DEF VAR ldCredLimit   AS DEC   NO-UNDO. 
DEF VAR ldOldCLimit   AS DEC   NO-UNDO. 
DEF VAR lcCLTemp      AS CHAR  NO-UNDO. 
DEF VAR llOpenCLI     AS LOG   NO-UNDO. 
DEF VAR lcCustRef     AS CHAR  NO-UNDO. 
DEF VAR lcInvRef      AS CHAR  NO-UNDO. 
DEF VAR lcLimitExtGrp AS CHAR  NO-UNDO.
DEF VAR lcVatUsage    AS CHAR  NO-UNDO. 
DEF VAR lcRepCode     AS CHAR  NO-UNDO. 
DEF VAR llDelNote     AS LOG   NO-UNDO EXTENT 7 FORMAT "*/".
DEF VAR liMainCust    AS INT   NO-UNDO INIT 0.
DEF VAR lcInfo        AS CHAR  NO-UNDO. 

DEF VAR llAgrCust     AS LOG   NO-UNDO.
DEF VAR llInvCust     AS LOG   NO-UNDO.
DEF VAR llOthInvCust  AS LOG   NO-UNDO. 
DEF VAR llUser        AS LOG   NO-UNDO.
DEF VAR lcAgrCust     AS CHAR  NO-UNDO.
DEF VAR lcInvCust     AS CHAR  NO-UNDO.
DEF VAR lcRoles       AS CHAR  NO-UNDO.
DEF VAR lcCustName    AS CHAR  NO-UNDO.
DEF VAR ldtChkDate    AS DATE  NO-UNDO. 
DEF VAR lcReject      AS CHAR  NO-UNDO. 
DEF VAR liFrmRow      AS INT   NO-UNDO.
DEF VAR liFrmDown     AS INT   NO-UNDO.
DEF VAR lcFrmTitle    AS CHAR  NO-UNDO.
DEF VAR lcChgTitle    AS CHAR  NO-UNDO.
DEF VAR liF2Type      AS INT   NO-UNDO. 
DEF VAR liF3Type      AS INT   NO-UNDO. 
DEF VAR lcPassword    AS CHAR  NO-UNDO. 
DEF VAR lcAskPassWd   AS CHAR  NO-UNDO.
DEF VAR lcIDType      AS CHAR  NO-UNDO.
DEF VAR lcRegion      AS CHAR  NO-UNDO.
DEF VAR lcNationality AS CHAR  NO-UNDO.
DEF VAR lcSex         AS CHAR  NO-UNDO.
DEF VAR lcDefCountry  AS CHAR  NO-UNDO.
DEF VAR lcBankName1   AS CHAR  NO-UNDO.
DEF VAR lcBankAddr    AS CHAR  NO-UNDO.
DEF VAR lcBankPost    AS CHAR  NO-UNDO.
DEF VAR lcSurName1    AS CHAR  NO-UNDO.
DEF VAR lcSurName2    AS CHAR  NO-UNDO.
DEF VAR lcCompany     AS CHAR  NO-UNDO.
DEF VAR lcCutLine     AS CHAR  NO-UNDO.
DEF VAR lcBankRef     AS CHAR  NO-UNDO.
DEF VAR pdMobSubLimit AS INT   NO-UNDO.
DEF VAR ldOrigMSLimit AS DEC   NO-UNDO.
DEF VAR llMSLimitIsDefault AS LOGICAL NO-UNDO.
DEF VAR pdMobSubActLimit AS INT   NO-UNDO.
DEF VAR ldOrigMSActLimit AS DEC   NO-UNDO.
DEF VAR llMSActLimitIsDefault AS LOG NO-UNDO.
DEF VAR lcInvTargetRule AS CHAR NO-UNDO. 
DEF VAR llAddressValidated AS LOG NO-UNDO. 
DEF VAR lcProfession  AS CHAR  NO-UNDO.
DEF VAR lcMemo        AS CHAR  NO-UNDO.
DEF VAR lcNWProfile   AS CHAR  NO-UNDO. /* RES-885 */
DEF VAR llAccess      AS LOG   NO-UNDO.
DEF VAR lcProgram     AS CHAR  NO-UNDO.

DEF VAR lcCustCOname  LIKE Customer.COName  NO-UNDO.
DEF VAR lcCustAddress LIKE Customer.Address  NO-UNDO.
DEF VAR lcCustZipCode LIKE Customer.ZipCode NO-UNDO.
DEF VAR lcCustRegion  LIKE Customer.Region  NO-UNDO.
DEF VAR lcCustCountry LIKE Customer.Country  NO-UNDO.
DEF VAR lcCustPostOffice LIKE Customer.PostOffice NO-UNDO.

lcProgram = PROGRAM-NAME(1).

IF iiCustNum > 0 AND NUM-ENTRIES(icType,"¤") > 1 THEN ASSIGN 
   lcChgTitle = ENTRY(2,icType,"¤")
   icType     = ENTRY(1,icType,"¤").
   
CASE icType:
WHEN "" OR WHEN "address_chg" THEN ASSIGN lcTyyppi   = "Brand"
                    liFrmRow   = 1
                    liFrmDown  = 15
                    lcFrmTitle = "CUSTOMERS".

OTHERWISE DO:
   ASSIGN lcTyyppi   = icType
          liMainCust = iiCustNum
          iiCustNum  = 0
          liFrmRow   = 6
          liFrmDown  = 10.
   CASE icType:
   WHEN "agrusers" OR
   WHEN "invusers" THEN lcFrmTitle = "USERS".
   WHEN "invcust"  THEN lcFrmTitle = "INV.CUSTOMERS".
   END CASE.
END.
END CASE.                 

/* does customer have special prices */
FUNCTION fChkPrices RETURNS LOGICAL.
   RETURN CAN-FIND(FIRST Tariff OF Customer).
END FUNCTION.

/* does customer have discounts */
FUNCTION fChkDiscount RETURNS LOGICAL.

   ok = FALSE. 
   FOR EACH BillTarg OF Customer NO-LOCK,
       FIRST DiscPlan NO-LOCK WHERE
             DiscPlan.Brand    = Customer.Brand AND
             DiscPlan.DiscPlan = BillTarg.DiscPlan,
       FIRST DpConf OF DiscPlan NO-LOCK:

      ok = TRUE.
   END.    

   RETURN ok.

END FUNCTION.

ASSIGN
   lcPassword    = fCParamC("MsAddressChg")
   lcLimitExtGrp = fCParamC("CustCredLimitExternalGrp")
   lcDefCountry  = fCParamC("CountryCodeDef")
   lcCutLine     = FILL("-",78)   
   lcMemo        = "Agent" + CHR(255) + "TMS".

IF lcPassword = ? THEN lcPassword = "".
 
form
    Customer.CustNum    format ">>>>>>>>9" column-label "CustNbr"
    lcCustName          format "x(30)"    column-label "Name"
    Customer.ZipCode    format "x(5)"     column-label "Zip"
    Customer.PostOffice format "x(10)"    column-label "Post.Addr"
    Customer.OrgId      format "x(11)"    column-label "PerID/ComID"
       help "Organization Code or Social Security Number"
    lcRoles             format "X(5)"     column-label "A I U"   
    memochr             format "M/"       column-label "M"
WITH width 80 OVERLAY ROW liFrmRow scroll 1 liFrmDown DOWN
    color value(Syst.Var:cfc) title color value(Syst.Var:ctc) 
       " " + Syst.Var:ynimi + " " + lcFrmTitle + " " +
       string(TODAY,"99-99-99") + " " FRAME sel.

form 
   {Mc/nnasse.frm}
WITH centered OVERLAY ROW 1 WIDTH 80
     side-labels TITLE fr-header FRAME lis.

form 
    skip(1)
    "eMail:"                           SKIP
    Customer.eMail     format "x(76)"  skip(1)
    "eMail (Mobile):"                  SKIP
    Customer.MobeMail FORMAT "x(76)"  skip(1)
    "eMail (Internet):"
    Customer.NeteMail  FORMAT "x(76)" 
    skip(1)
WITH centered OVERLAY ROW 4 no-LABELs TITLE " eMail Addresses " 
     FRAME femails.

{Func/brand.i}

form /* Customer :n tunnuksella hakua varten */
   "Brand Code:" lcBrand  HELP "Enter Brand" 
      VALIDATE(lcBrand = "*" OR CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
               "Unknown brand")
      SKIP
   "Number ...:" CustNum help "Enter Customer Number"
with row 4 col 2 title color value(Syst.Var:ctc) " FIND CUST No. "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME search-1.

form /* Customer :n nimella hakua varten */
   "Brand Code:" lcBrand  HELP "Enter Brand" 
      VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
               "Unknown brand")
      SKIP
  "FirstName.:" lcFirstName FORMAT "X(20)"
     HELP "First name"
  SKIP
  "SurName1..:" lcSurName1 FORMAT "x(30)"
     HELP "Customers 1st Surname" 
  SKIP
  "SurName2..:" lcSurName2 FORMAT "x(30)"
     HELP "Customers 2nd Surname"
  SKIP
  "Company...:" lcCompany  FORMAT "X(30)"
     HELP "Company name" SKIP
  with row 4 col 2 title color value(Syst.Var:ctc) " FIND Name "
  COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME search-2.
 
form /* Aakkoshakua varten */
   "Brand Code:" lcBrand  HELP "Enter Brand" 
      VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
               "Unknown brand")
      SKIP
   "Abbreviat.:" SearchName help "Enter abbreviation"
with row 4 col 2 title color value(Syst.Var:ctc) " FIND ABBREVIATION "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME search-3.

form /* FIND zip */
   "Brand Code:" lcBrand  HELP "Enter Brand" 
      VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
               "Unknown brand")
      SKIP
   "Zip Code .:" lcZip  help "Enter ZIP code"
with row 4 col 2 title color value(Syst.Var:ctc) " FIND ZIP "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME search-4.

form /* FIND OrgCode */
   "Brand Code:" lcBrand  HELP "Enter Brand" 
      VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
               "Unknown brand")
      SKIP
   "PersonID .:" OrgId  help "Enter personID or company ID"
with row 4 col 2 title color value(Syst.Var:ctc) " FIND PERSONID "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME search-6.

form /* FIND CLI # */
   "Brand Code:" lcBrand  HELP "Enter Brand" 
      VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
               "Unknown brand")
      SKIP
   "CLI ......:" CLI help "Enter A-number or beginning of it"
with row 4 col 2 title color value(Syst.Var:ctc) " FIND CLI "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME search-7.


form
   mess[1]  NO-LABEL SKIP
   mess[2]  NO-LABEL SKIP
   mess[3]  NO-LABEL SKIP
   mess[4]  NO-LABEL skip(1)
   mess[5]  NO-LABEL SKIP
WITH OVERLAY centered ROW 5 FRAME frm.

FORM 
   Customer.IDelName        COLON 15 LABEL "Name" 
   Customer.IDelCOName      COLON 15 LABEL "Addit.Name"
   Customer.IDelAddr        COLON 15 LABEL "Address"
   Customer.IDelZipCode     COLON 15 LABEL "Postal Addr."
      Customer.iDelPost NO-LABEL 
   Customer.iDelCountry     COLON 15 LABEL "Country" 
   WITH SIDE-LABELS OVERLAY ROW 12 CENTERED 
        TITLE " Invoice Delivery Address " FRAME fInvDel.
 
form
   "Copy from ...:" xCustomer.CustNum xCustomer.CustName SKIP
   "OK to copy ..:" ok
WITH 
   title " Copy customer data from ... "
   OVERLAY centered ROW 7 FRAME copy NO-LABELS.

form
   Customer.CustNum     
      label "Customer ....." 
      lcCustName FORMAT "X(40)" NO-LABEL 
      SKIP

   lcBankAcc            
      label "Bank Account ." 
      FORMAT "x(30)"  
      HELP "Bank account" 
      SKIP
   lcBankName1 AT 17 
      NO-LABEL
      FORMAT "X(40)" 
      SKIP
   lcBankAddr AT 17 
      NO-LABEL
      FORMAT "X(40)" 
      SKIP
   lcBankPost AT 17
      NO-LABEL
      FORMAT "X(40)" 
      SKIP(1)

   Customer.ClaimPerm  
      LABEL "Claiming Perm." 
      SKIP
   Customer.InterestPerm 
      LABEL "Interest Perm." 
      SKIP
   Customer.InvoiceTargetRule
      LABEL "Inv.TargetRule"
      lcInvTargetRule FORMAT "x(15)" NO-LABEL
   Customer.CreditLimit AT 50
      LABEL "ODI Limit"
      FORMAT ">>>>9.99"
      HELP "Limit for creating on demand invoices"
      SKIP 
 
   lcCutLine AT 1 
      NO-LABEL
      FORMAT "X(78)"
      SKIP
      
   Customer.ChargeType    
      label "Charge Type .." 
      validate(INPUT Customer.ChargeType >= 1 AND 
               INPUT Customer.ChargeType <= 6,
               "Valid values are 1 - 6")
      lcCType NO-LABEL FORMAT "X(25)" 
      SKIP
   Customer.PaymTerm    
      label "Payment Term. " "working day of month"     
      SKIP
   Customer.DelType       
      label "Delivery Type " 
      format ">9"
      lcDType NO-LABEL FORMAT "X(35)" 
      SKIP(1)
      
   Customer.Currency
      label "Currency ....."  SKIP
   Customer.VATUsage    
      label "Tax Usage ...." 
      lcVatUsage NO-LABEL FORMAT "X(40)" skip
   Customer.VATIncl     
      label "Tax on Invoice"  
      HELP  "Format for invoices; is VAT (I)ncluded or (E)xcluded in amounts"
      SKIP
WITH
   WIDTH 80 overlay title " Billing Data " side-labels
   ROW 1 FRAME fina.

FUNCTION fDispCustomer RETURNS LOGICAL:

   FIND FIRST CustomerReport WHERE
      CustomerReport.CustNum = Customer.Custnum NO-LOCK NO-ERROR.
   IF AVAIL CustomerReport AND
            CustomerReport.StreetCode > "" THEN llAddressValidated = TRUE.

   /* RES-885 NRTR National roaming traffic restriction */
   IF Customer.NWProfile EQ 1 THEN
      lcNWProfile = "1 (YG)".
   ELSE IF Customer.NWProfile EQ 2 THEN
      lcNWProfile = "2 (YG+OR)".
   ELSE IF Customer.NWProfile EQ 3 THEN
      lcNWProfile = "3 (YG+OR+TE)".
   ELSE
      lcNWProfile = "".

DISP 
  
   Customer.CustIDType
   Customer.OrgId 
   Customer.BirthDay
   Customer.CustName 
   Customer.SurName2
   Customer.HonTitle
   Customer.FirstName
   Customer.CompanyName
   Customer.Profession lcProfession
   Customer.COName @ lcCustCOName
   lcNWProfile
   Customer.Address @ lcCustAddress 
   Customer.AuthCustIdType WHEN Customer.CustIDType = "CIF"
   Customer.ZipCode @ lcCustZipCode 
   Customer.AuthCustId WHEN Customer.CustIDType = "CIF"
   Customer.PostOffice @ lcCustPostOffice
   Customer.Country @ lcCustCountry lcCountry
   Customer.Region @ lcCustRegion lcRegion
   Customer.Nationality lcNationality
   Customer.InvGroup
   Customer.Category  dkatnimi
   Customer.Language 
   Customer.CustNum 
   llAddressValidated

   dkatnimi  
   lcLang
   pdMobSubLimit 
   llMSLimitIsDefault 
   pdMobSubActLimit
   llMSActLimitIsDefault
   llAgrCust lcAgrCust
   llInvCust lcInvCust
   llOthInvCust
   llUser
   Customer.CreDate
   Customer.DataProtected
   WITH FRAME lis.

IF Customer.CustIDType NE "CIF" THEN HIDE 
   Customer.AuthCustIdType 
   Customer.AuthCustId
   IN FRAME lis.

   RETURN TRUE.

END FUNCTION. 


FUNCTION fDispVATUsage RETURNS LOGICAL
   (iiVATUsage AS INT).
   
   lcVatUsage = Func.Common:mTMSCodeName("Invoice",
                                 "VATUsage",
                                 STRING(iiVatUsage)).   
   
   DISPLAY lcVATUsage WITH FRAME fina.
   
   RETURN (lcVatUsage > ""). 
   
END FUNCTION.

FUNCTION fTempCredLimit RETURNS LOGICAL
   (ilCLUsed AS LOG).

   IF ilCLUsed AND CAN-FIND(FIRST AddCustLimit WHERE
                                  AddCustLimit.CustNUm = Customer.CustNum AND
                                  AddCustLimit.dTo   >= TODAY)
   THEN lcCLTemp = "!".
   ELSE lcCLTemp = "".

END FUNCTION.

FUNCTION fCustSex RETURNS LOGICAL
   (icSex AS CHAR):
   
   CASE icSex:
   WHEN "F" THEN lcSex = "Female".
   WHEN "M" THEN lcSex = "Male".
   OTHERWISE     lcSex = "".
   END CASE. 
    
END FUNCTION.

FUNCTION fLocalCustRoles RETURNS LOGICAL.

   lcInfo = fCustRoles(BUFFER Customer).
   
   ASSIGN llAgrCust    = (SUBSTRING(lcInfo,1,1) = "1")
          llInvCust    = Customer.InvCust = Customer.CustNum
          llOthInvCust = (SUBSTRING(lcInfo,2,1) = "1")
          llUser       = (SUBSTRING(lcInfo,3,1) = "1").

END FUNCTION.

FUNCTION fRegion RETURNS LOGICAL
   (icRegion AS CHAR):
   
   lcRegion = "".
   
   IF icRegion > "" THEN DO:
      FIND Region WHERE Region.Region = icRegion NO-LOCK NO-ERROR.
      IF NOT AVAIL Region THEN RETURN FALSE.

      lcRegion = Region.RgName.
   END.

   RETURN TRUE.
   
END FUNCTION.

FUNCTION fCountry RETURNS LOGICAL
   (icCountry AS CHAR):
   
   lcCountry = "".
   
   IF icCountry > "" THEN DO:
      FIND Country WHERE Country.Country = icCountry NO-LOCK NO-ERROR.
      IF NOT AVAIL Country THEN RETURN FALSE.

      lcCountry = Country.CoName.
   END.

   RETURN TRUE.
   
END FUNCTION.

FUNCTION fLanguage RETURNS LOGICAL
   (iiLanguage AS INTEGER):
        
   FIND Language WHERE Language.Language = iiLanguage 
   NO-LOCK NO-ERROR.
   IF AVAIL Language THEN ASSIGN lcLang = Language.LangName.
   ELSE lcLang = "".

   RETURN TRUE.

END FUNCTION. 

FUNCTION fCustCat RETURNS LOGICAL
   (icCategory AS CHARACTER):

   dkatnimi = "".
   FIND FIRST CustCat where 
             CustCat.Brand    = Syst.Var:gcBrand AND
             CustCat.Category = icCategory 
   no-lock no-error.
   IF AVAIL CustCat THEN ASSIGN dkatnimi = CustCat.CatName.

   RETURN TRUE.

END FUNCTION. 

FUNCTION fNationality RETURNS LOGICAL
   (icNationality AS CHAR):
   
   lcNationality = "".
   
   IF icNationality > "" THEN DO:
      FIND Nationality WHERE 
           Nationality.Nationality = icNationality NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Nationality THEN RETURN FALSE.
      
      lcNationality = Nationality.NtName.
   END.
      
   RETURN TRUE.

END FUNCTION.

FUNCTION fProfession RETURNS LOGICAL
   (icProfession AS CHAR):
   
   lcProfession = "".
   
   IF icProfession > "" THEN DO:
      FIND FIRST TMSCodes WHERE
                 TMSCodes.TableName = "OrderCustomer" AND
                 TMSCodes.FieldName = "Profession"    AND
                 TMSCodes.CodeValue = icProfession NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TMSCodes THEN RETURN FALSE.
      
      lcProfession = TMSCodes.CodeName.
   END.
      
   RETURN TRUE.

END FUNCTION.

mess[1] = "This customer has:".
mess[4] = "where starting Amount is allowed.".
mess[5] = "This overrides all those settings.".
                      

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc. view FRAME sel.

ASSIGN
   month    = (year(TODAY) * 100) + month(TODAY)
   order    = 1 .

IF iiCustNum > 0 THEN lctyyppi = "INPUTCUST".

RUN pFindFirst.

IF AVAIL Customer THEN
   ASSIGN 
      memory     = recid(Customer) 
      must-print = TRUE 
      must-add   = FALSE.
ELSE DO:
  IF lcRight NE "RW" THEN DO:
     MESSAGE "No Customers available !" VIEW-AS ALERT-BOX.
     fCleanEventObjects(). 
     RETURN.
  END.
  ELSE ASSIGN  
     memory     = ? 
     must-print = FALSE 
     must-add   = FALSE.
END.     

ASSIGN 
   xrecid    = ? 
   delline   = 0 
   ufkey     = TRUE 
   firstline = 0.

IF icType = "address_chg" AND lcRight = "RW" THEN DO:    

   DEFINE VARIABLE liReq AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO.

   FIND Customer WHERE Customer.Custnum = iiCustNum NO-LOCK NO-ERROR.

   fr-header = " CUSTOMER ADDRESS CHANGE ".
   Syst.Var:ehto = 9. RUN Syst/ufkey.p.

   /* UPDATE Customer record */

   pdMobSubLimit = fGetMobSubLimit(Customer.Custnum, 
                                   Customer.Category, 
                            OUTPUT llMSLimitIsDefault).

   pdMobSubActLimit = fGetMobSubActLimit(Customer.Custnum, 
                                         Customer.Category, 
                                         OUTPUT llMSActLimitIsDefault).

   fDispCustomer().
   
   ASSIGN 
      lcCustCOName = Customer.COName
      lcCustAddress = Customer.Address
      lcCustZipCode = Customer.ZipCode
      lcCustPostOffice = Customer.PostOffice
      lcCustCountry  = Customer.Country
      lcCustRegion = Customer.Region.
  
   ADDRESS_UPDATE:
   REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:
       
      ASSIGN ldOrigMSLimit    = pdMobSubLimit
             ldOrigMSActLimit = pdMobSubActLimit.
       
      fCountry(lcCustCountry).
      fCustCat(Customer.Category).
      fLanguage(Customer.Language).
      fRegion(lcCustRegion).
      fNationality(Customer.Nationality).
      pdMobSubLimit = fGetMobSubLimit(Customer.Custnum, 
                                      Customer.Category, 
                               OUTPUT llMSLimitIsDefault).
      pdMobSubActLimit = fGetMobSubActLimit(Customer.Custnum, 
                                            Customer.Category, 
                                            OUTPUT llMSActLimitIsDefault).
       
      DISP 
          lcCountry  
          lcRegion  
          lcLang  
          lcNationality
          dkatnimi
          Customer.Category
      WITH FRAME lis.
       
      UPDATE
       
       lcCustCOName 
       lcCustAddress 
       lcCustZipCode 
       lcCustPostOffice
       lcCustRegion
       
       WITH FRAME lis
       {Mc/nnasse.i}

       /* cross reference checks */  
       IF Customer.CustNum > 1000 THEN DO:

         IF 
            lcCustCOName = Customer.COName AND
            lcCustAddress = Customer.Address AND
            lcCustZipCode = Customer.ZipCode AND
            lcCustPostOffice = Customer.PostOffice AND
            lcCustCountry  = Customer.Country AND 
            lcCustRegion = Customer.Region THEN DO:
            MESSAGE "Address data is not changed" VIEW-AS ALERT-BOX.
            NEXT ADDRESS_UPDATE.
         END.

         MESSAGE
            "Do You really want to create address change request?"
         VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE " CONFIRMATION " UPDATE ok .
         
         IF NOT ok THEN NEXT ADDRESS_UPDATE.                        

            liReq = fAddressRequest(
               Customer.Custnum,
               0,
               lcCustAddress,
               lcCustZipCode,
               lcCustPostOffice,
               lcCustRegion,
               lcCustCountry,
               lcCustConame,
               "",
               "",
               "",
               "4",
               0,
               OUTPUT ocResult).
            
            IF ocResult NE "" THEN DO:
               MESSAGE ocResult VIEW-AS ALERT-BOX.
               
            END.

            MESSAGE
               "Request ID for address change is:" liReq
            VIEW-AS ALERT-BOX TITLE " REQUEST ADDED ".            
        
         LEAVE.
      END.   
   END.
   fCleanEventObjects(). 
   RETURN.

END.  /* Syst.Var:toimi = 1 */


LOOP:
repeat WITH FRAME sel:

   IF order <> ex-order THEN DO:
      ex-order = order. 
       /*
       if order = 1 then put screen row 19 col 30 "   Order by number    ".
       if order = 2 then put screen row 19 col 30 "    Order by Name     ".
       if order = 3 then put screen row 19 col 30 "   Order by ZipCode   ".
       if order = 4 then put screen row 19 col 30 "   Order by Org Code  ".
       */
   END.

   IF must-add THEN DO:  /* asiakkaa -ADD  */
      ASSIGN ufkey = TRUE must-add = FALSE must-print = TRUE
      fr-header = " NEW CUSTOMER ". RUN Syst/ufcolor.p.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         new_custNo = 1.
         RUN Help/custser.p.

         i = index(siirto,"-").
         IF i > 0 THEN DO:
            ASSIGN
               x1 = int(substr(siirto, 1, i - 1))
               x2 = int(substr(siirto, i + 1)).

            FIND LAST xCustomer NO-LOCK WHERE
                      xCustomer.CustNum >= x1 AND
                      xCustomer.CustNum <= x2 NO-ERROR.
            IF AVAILABLE xCustomer THEN ASSIGN
               x1 = xCustomer.CustNum. 

            DO new_custNo = x1 TO x2:
               IF NOT can-find(FIRST Customer where
                                     Customer.CustNum = new_custNo) THEN LEAVE.
            END.
         END.
         ELSE DO:
            UNDO add-new, LEAVE add-new.
            /*
            {Mc/custfind.i LAST CustNum}
            IF AVAIL Customer THEN new_custNo = Customer.CustNum + 1. */
         END. 

         lcInvGroup = "".
         CLEAR FRAME lis no-pause. Syst.Var:ehto = 9. RUN Syst/ufkey.p.

         RUN Syst/ufxkey.p(5,964).

         DO TRANS ON ENDKEY UNDO, LEAVE:
           CLEAR FRAME lis no-pause.
           DISPLAY new_custNo @ Customer.CustNum.
           
           Ok = FALSE.
           MESSAGE "Do You want to change the default number for new"
                   "customer" SKIP
                   "(change requires a password)?"
           VIEW-AS ALERT-BOX QUESTION
           BUTTONS YES-NO-CANCEL
           TITLE " NUMBER FOR NEW CUSTOMER "
           SET Ok.
           
           IF Ok = ? THEN LEAVE add-new.
           
           IF Ok THEN DO:
              
              IF lcPassword > "" THEN DO:
                 lcAskPassWd = "".
            
                 PAUSE 0.
                 UPDATE lcAskPassWd 
                        BLANK
                        FORMAT "X(20)" 
                        LABEL "Password"
                    HELP "Password for changing default number" 
                 WITH OVERLAY ROW 10 CENTERED TITLE " NEW CUSTOMER "
                 SIDE-LABELS FRAME fPassword.
           
                 HIDE FRAME fPassword NO-PAUSE.           
                 IF lcAskPassWd NE lcPassword THEN DO:
                    MESSAGE "Incorrect password, add cancelled"
                    VIEW-AS ALERT-BOX INFORMATION.
                    LEAVE add-new.
                 END.
              END.
              
              PROMPT-FOR Customer.CustNum
              VALIDATE (Customer.CustNum = 0 OR
                 NOT can-find(Customer using  INPUT FRAME lis Customer.CustNum),
                 "Customer '" +
                 string(input frame lis Customer.CustNum) + 
                 "' already exists !").
           END.
           
           IF INPUT FRAME lis Customer.CustNum = 0 THEN LEAVE add-new.

           REPEAT ON ENDKEY UNDO add-new, LEAVE add-new:
              PAUSE 0.
              UPDATE lcInvGroup 
                 NO-LABEL 
                 HELP "Invoicing group that the new is customer is added to"
                 WITH OVERLAY ROW 10 CENTERED TITLE " Invoicing Group "
                 FRAME fGroup.

              FIND InvGroup NO-LOCK WHERE 
                   InvGroup.InvGroup = lcInvGroup AND 
                   InvGroup.Brand    = Syst.Var:gcBrand NO-ERROR.
                   
              IF NOT AVAILABLE InvGroup THEN DO:
                 MESSAGE "Unknown invoicing group"
                 VIEW-AS ALERT-BOX
                 ERROR.
              END.
              ELSE DO:
                 HIDE FRAME fGroup NO-PAUSE.
                 LEAVE.
              END. 
           END.    

           CREATE Customer.

           liDefCust = fCParamI("DefCust" + lcInvGroup + "/1").
           IF liDefCust NE ? AND liDefCust > 0 THEN DO:
              FIND BaseCust NO-LOCK WHERE
                   BaseCust.Brand   = lcBrand AND
                   BaseCust.CustNum = liDefCust NO-ERROR.
              IF AVAILABLE BaseCust AND 
                 BaseCust.InvGroup = lcInvGroup
              THEN DO:
                 BUFFER-COPY BaseCust 
                    EXCEPT CustNum SearchName CustName COName OrgId
                           UpdDate UpdUser ChgStamp ExpStamp
                 TO Customer.

                 FOR EACH BaseTarget OF BaseCust NO-LOCK:
                    CREATE BillTarget.
                    BUFFER-COPY BaseTarget EXCEPT CustNum TO BillTarget.
                    BillTarget.CustNum = INPUT FRAME lis Customer.CustNum.
                 END.
              END.
           END.

           ASSIGN
              Customer.ChgStamp = Func.Common:mMakeTS()
              Customer.Brand    = lcBrand 
              Customer.CreUser  = Syst.Var:katun
              Customer.CreDate  = TODAY  
              Customer.CustNum  = INPUT FRAME lis Customer.CustNum
              Customer.InvCust  = Customer.CustNum 
              Customer.PaymCust = Customer.CustNum
              Customer.RepCust  = Customer.CustNum
              Customer.RateCust = Customer.CustNum 
              Customer.AgrCust  = Customer.CustNum
              Customer.ContrBeg = TODAY
              xrecid            = recid(Customer).

           FIND CustCat WHERE 
                CustCat.Brand    = Syst.Var:gcBrand AND
                CustCat.Category = Customer.Category 
           NO-LOCK NO-ERROR.
           IF AVAILABLE CustCat THEN Customer.PaymTerm = CustCat.PaymTerm.

           release Customer.
           FIND FIRST Customer where
                recid(Customer) = xrecid
           exclusive-lock.


           REPEAT WITH FRAME LIS:
              if keylabel(lastkey) NE "F4" THEN RUN local-update-customer.
              if keylabel(lastkey) =  "F4" THEN DO:
                 must-add = FALSE.
                 UNDO add-new, LEAVE add-new.
              END.
              IF Customer.CustName = "" OR
                 Customer.InvGroup = ""
                 THEN DO:

                 MESSAGE 
                 "Customer record is yet incomplete - " SKIP
                 "Complete the missing fields"
                 VIEW-AS ALERT-BOX ERROR.
                 NEXT.
              END.   

              LEAVE.
           END.   
           memory = recid(Customer).

           /* set the default VALUE of CreditInvNum Limit */
           FIND FIRST TMSParam WHERE 
               TMSParam.Brand = Syst.Var:gcBrand AND
               TMSParam.ParamCode = "CreditLimit:" + Customer.Category
           no-lock no-error.
           IF AVAIL TMSParam THEN Customer.CreditLimit = tmsparam.intVal.

         END. /* lisAA-transactio */

         PAUSE 0.
         if keylabel(lastkey) NE "F4" THEN DO:

            PAUSE 0.
            message "Next the FINANCIAL things ..."
            VIEW-AS ALERT-BOX INFORMATION.
            RUN local-update-fin.

         END.

         /*release Customer.*/

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustomer).

         LEAVE.
      END.
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.
      /* any records AVAILABLE ? */
      {Mc/custfind.i FIRST CustNum}
      IF NOT AVAILABLE Customer THEN LEAVE LOOP.
      NEXT LOOP.
   END.


   print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND Customer where recid(Customer) = memory no-lock no-error.
         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */
         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.
         repeat WITH FRAME sel:
            IF AVAILABLE Customer THEN DO:

               RUN local-disp-row. 

               rtab[FRAME-LINE] = recid(Customer).

               RUN pFindNext.

            END.
            ELSE DO:  CLEAR no-pause.  rtab[FRAME-LINE] = ?. END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE. DOWN.
         END.
         UP FRAME-LINE - 1.  
         DOWN firstline.
         ASSIGN 
            firstline = 0 
            must-print = FALSE.
         PAUSE 0 no-message.
         /* one page of data has been Printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1. ASSIGN delline = 0.


   BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:
      
      IF ufkey AND iiCustnum = 0 THEN DO:
         ASSIGN 
         Syst.Var:ufk = 0 
         Syst.Var:ufk[1] = 714
         Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 732  ELSE 0)
         Syst.Var:ufk[6]= (IF lcRight = "RW" THEN 4    ELSE 0)
         Syst.Var:ufk[7]= 0 
         Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
         Syst.Var:ehto = 3 ufkey = FALSE.  
         
         IF liMainCust > 0 THEN ASSIGN
            Syst.Var:ufk    = 0    
            Syst.Var:ufk[8] = 8.
         
         RUN Syst/ufkey.p.
      END.
      
      IF iiCustnum = 0 THEN DO:
         HIDE MESSAGE no-pause. 
         IF order = 1 THEN
            CHOOSE ROW Customer.CustNum {Syst/uchoose.i} no-error WITH FRAME sel.
         ELSE IF order = 2 THEN
            CHOOSE ROW lcCustName {Syst/uchoose.i} no-error WITH FRAME sel.
         ELSE IF order = 3 THEN
            CHOOSE ROW Customer.ZipCode {Syst/uchoose.i} no-error WITH FRAME sel.
         ELSE IF order = 4 THEN
            CHOOSE ROW Customer.OrgId {Syst/uchoose.i} no-error WITH FRAME sel.
       
         COLOR DISPLAY value(Syst.Var:ccc) 
         Customer.CustNum Customer.ZipCode
         Customer.OrgId lcCustName
         WITH FRAME sel.
      END.
      
      IF iiCustnum > 0 THEN Syst.Var:nap = "enter".
      ELSE ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-LINE] = ? AND 
         LOOKUP(Syst.Var:nap,"f5,5,f8,8") = 0
      THEN DO:
         bell. message "Move upwards !".
         PAUSE 1 no-message. NEXT.
      END.

      if lookup(Syst.Var:nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 5 THEN order = 1. 
      END.
      if lookup(Syst.Var:nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 4. 
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND Customer where recid(Customer) = memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:

            RUN pFindPrev.

            IF AVAILABLE Customer THEN
               ASSIGN firstline = i memory = recid(Customer).
            ELSE LEAVE.
         END.
         must-print = TRUE. NEXT LOOP.
      END.

        /* previous line */
      if lookup(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND Customer where recid(Customer) = rtab[1] no-lock.

            RUN pFindPrev.

            IF NOT AVAILABLE Customer THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message. NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.

               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 BY -1:  rtab[i] = rtab[i - 1]. END.
               ASSIGN rtab[1] = recid(Customer) memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND Customer where recid(Customer) = rtab[FRAME-DOWN] no-lock .

            RUN pFindNext.

            IF NOT AVAILABLE Customer THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.  PAUSE 1 no-message. NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               RUN local-disp-row.

               DO i = 1 TO FRAME-DOWN - 1: rtab[i] = rtab[i + 1].  END.
               rtab[FRAME-DOWN] = recid(Customer).
               /* finally LAST line's KeyValue is saved */
               ASSIGN memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND Customer where recid(Customer) = memory no-lock no-error.

         RUN pFindPrev.

         IF AVAILABLE Customer THEN DO:
            memory = recid(Customer).
            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):

               RUN pFindPrev.

               IF AVAILABLE Customer THEN memory = recid(Customer).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE. NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".  BELL. PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(Syst.Var:nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE". BELL. PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND Customer where recid(Customer) = memory no-lock.
            must-print = TRUE. NEXT LOOP.
        END.
     END. /* NEXT page */


     /**************************************
     * Search functions in a separate loop *
     ***************************************/

     else if lookup(Syst.Var:nap,"f1,1") > 0 AND Syst.Var:ufk[1] > 0
     THEN repeat WITH FRAME sel:

        ASSIGN 
        ufkey = TRUE Syst.Var:ufk = 0 Syst.Var:ehto = 1
        Syst.Var:ufk[1] = 707  Syst.Var:ufk[2] = 30  
        Syst.Var:ufk[3] = 1050 Syst.Var:ufk[4] = 812 
        Syst.Var:ufk[8] = 8.
        
        RUN Syst/ufkey.p.
        IF Syst.Var:toimi = 8 THEN NEXT BROWSE.

        /* Search 1 */
        IF Syst.Var:toimi = 1 THEN DO:  /* search WITH column 1 */
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           CustNum = 0. Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.

           PAUSE 0.
           DISP lcBrand WITH FRAME search-1. 
           UPDATE lcBrand WHEN Syst.Var:gcAllBrand = TRUE 
                  CustNum WITH FRAME search-1.

           HIDE FRAME search-1 no-pause.
           IF CustNum <> 0 THEN DO:

              IF lcBrand = "*" THEN lcTyyppi = "".

              {Mc/custfind.i FIRST CustNum "AND Customer.CustNum >= CustNum"}.

              ASSIGN lcTyyppi = "brand".

              IF NOT fRecFound(1) THEN NEXT BROWSE.

              llAccess = TRUE.

              NEXT LOOP.
           END.
        END. /* Search col. 1 */

        /* Search WITH column 2 */
        ELSE  IF Syst.Var:toimi = 2 THEN DO:  /* search col. 3 */
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p. 
           
           ASSIGN
              lcFirstName = ""
              lcSurName1  = ""
              lcSurName2  = ""
              lcCompany   = "".

           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISP lcBrand WITH FRAME search-2. 
           UPDATE
              lcBrand WHEN Syst.Var:gcAllBrand = TRUE 
              lcFirstName
              lcSurName1
              lcSurName2
              lcCompany
           WITH FRAME search-2.
           HIDE FRAME search-2 no-pause.

           IF lcFirstName NE "" OR
              lcSurName1  NE "" OR
              lcSurName2  NE "" OR
              lcCompany   NE "" THEN DO:
              
              /* one field defined */
              IF lcSurName1 = "" AND lcSurName2 = "" AND lcCompany = "" THEN DO:
                 {Mc/custfind.i FIRST CustName  "AND Customer.FirstName MATCHES '*' + lcFirstName + '*'"}.
              END.
              ELSE IF lcFirstName = "" AND lcSurName2 = "" AND lcCompany = "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.CustName >= lcSurName1"}.
              END.
              ELSE IF lcFirstName = "" AND lcSurName1 = "" AND lcCompany = "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.SurName2 >= lcSurName2"}.
              END.
              ELSE IF lcFirstName = "" AND lcSurName1 = "" AND lcSurName2 = "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.Company MATCHES '*' + lcCompany + '*'"}.
              END.
              /* two fields defined */
              ELSE IF lcFirstName NE "" AND lcSurName1 NE "" AND lcSurName2 = "" AND lcCompany = "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.FirstName MATCHES '*' + lcFirstName + '*'" 
                            + "AND Customer.CustName  >= lcSurName1"}.
              END.
              ELSE IF lcFirstName = "" AND lcSurName1 NE "" AND lcSurName2 NE "" AND lcCompany = "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.CustName  >= lcSurName1
                              AND Customer.SurName2  >= lcSurName2"}.
              END.
              ELSE IF lcFirstName = "" AND lcSurName1 = "" AND lcSurName2 NE "" AND lcCompany NE "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.SurName2  >= lcSurName2
                              AND Customer.Company MATCHES '*' + lcCompany + '*'"}.
              END.
              /* three fields defined */
              ELSE IF lcFirstName NE "" AND lcSurName1 NE "" AND lcSurName2 NE "" AND lcCompany = "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.FirstName  MATCHES '*' + lcFirstName + '*'"
                              AND Customer.CustName  >= lcSurName1
                              AND Customer.SurName2  >= lcSurName2"}.
              END.
              ELSE IF lcFirstName = "" AND lcSurName1 NE "" AND lcSurName2 NE "" AND lcCompany NE "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.CustName  >= lcSurName1
                              AND Customer.SurName2  >= lcSurName2
                              AND Customer.Company   MATCHES '*' + lcCompany + '*'"}.
              END.
              ELSE IF lcFirstName NE "" AND lcSurName1 = "" AND lcSurName2 NE "" AND lcCompany NE "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.FirstName  MATCHES '*' + lcFirstName + '*'"
                              AND Customer.SurName2  >= lcSurName2
                              AND Customer.Company MATCHES '*' + lcCompany + '*'"}.
              END.
              ELSE IF lcFirstName NE "" AND lcSurName1 NE "" AND lcSurName2 = "" AND lcCompany NE "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.FirstName MATCHES '*' + lcFirstName + '*'"
                              AND Customer.CustName  >= lcSurName1
                              AND Customer.Company MATCHES '*' + lcCompany + '*'"}.
              END.
              ELSE IF lcFirstName NE "" AND lcSurName1 NE "" AND lcSurName2 NE "" AND lcCompany = "" THEN DO:
                 {Mc/custfind.i FIRST CustName 
                             "AND Customer.FirstName MATCHES '*' + lcFirstName + '*'"
                              AND Customer.CustName  >= lcSurName1
                              AND Customer.SurName2  >= lcSurName2"}.
              END.
              

              IF NOT fRecFound(2) THEN NEXT BROWSE.

              llAccess = TRUE.

              NEXT LOOP.

           END.
           
        END. /* Search col. 2 */

        /* Search WITH column 3 */
        ELSE IF Syst.Var:toimi = 3 THEN DO: 
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p. lcZip = "".
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISP lcBrand WITH FRAME search-4. 
           UPDATE lcBrand WHEN Syst.Var:gcAllBrand = TRUE 
                  lcZip WITH FRAME search-4.
           HIDE FRAME search-4 no-pause.

           if lcZip <> "" THEN DO:

              {Mc/custfind.i FIRST ZipCode "AND Customer.ZipCode >= lcZip"}.

              IF NOT fRecFound(3) THEN NEXT BROWSE.

              llAccess = FALSE.

              NEXT LOOP.
           END.
        END.

        /* Search WITH column 4 */
        ELSE IF Syst.Var:toimi = 4 THEN DO: 
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p. OrgId = "".
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISP lcBrand WITH FRAME search-6. 
           UPDATE lcBrand WHEN Syst.Var:gcAllBrand = TRUE 
                  OrgId WITH FRAME search-6.
           HIDE FRAME search-6 no-pause.
           if OrgId <> "" THEN DO:

              {Mc/custfind.i FIRST OrgId "AND Customer.OrgId >= OrgId"}.

              IF NOT fRecFound(4) THEN NEXT BROWSE.

              llAccess = TRUE.

              NEXT LOOP.
           END.
        END.


     END.  /* search */

     ELSE IF LOOKUP(Syst.Var:nap,"4,F4") > 0 AND Syst.Var:ufk[4] > 0 AND lcRight = "RW" THEN DO:
        FIND Customer WHERE recid(Customer) = rtab[FRAME-LINE] NO-LOCK.
        CustNum = Customer.CustNum.

        RUN Mc/copycu.p(INPUT-OUTPUT CustNum, INPUT debug).
        IF CustNum NE Customer.CustNum THEN DO:
           FIND Customer WHERE Customer.CustNum = CustNum NO-LOCK.
           must-print = TRUE.
           ufkey = TRUE.
           memory = RECID(Customer).
           NEXT loop.
        END.
     END.

     else if lookup(Syst.Var:nap,"5,f5") > 0 THEN  /* lisays */
         MESSAGE "This function is not allowed." VIEW-AS ALERT-BOX.

     else if lookup(Syst.Var:nap,"enter,return") > 0 THEN 
     CHG: 
     REPEAT WITH FRAME lis:
     
        hide frame lis no-pause.
        FIND Customer where recid(Customer) = rtab[frame-line(sel)]
        no-lock no-error.

        FIND FIRST InvGroup OF Customer no-lock no-error.
        IF AVAIL InvGroup THEN igname = InvGroup.IGName.
        else igname = "".

        fCustCat(Customer.Category).
        
        fCountry(Customer.Country).

        fNationality(Customer.Nationality).
        fProfession(Customer.Profession).
        
        pdMobSubLimit = fGetMobSubLimit(Customer.Custnum, 
                                        Customer.Category, 
                                 OUTPUT llMSLimitIsDefault).
        pdMobSubActLimit = fGetMobSubActLimit(Customer.Custnum, 
                                              Customer.Category, 
                                              OUTPUT llMSActLimitIsDefault).
        
        fLanguage(Customer.Language).

        fCustSex(Customer.Sex).
       
        lcIDType = Func.Common:mTMSCodeName("Customer",
                                    "CustIDType",
                                    Customer.CustIdType).
        fRegion(Customer.Region).

        fLocalCustRoles().

        ASSIGN lcAgrCust = ""
               lcInvCust = "".
 
        /* agreement customer */
        IF NOT llAgrCust THEN DO:
           lcAgrCust = STRING(Customer.AgrCust,">>>>>>>9").
                
           FIND xCustomer where xCustomer.CustNum = Customer.AgrCust 
              no-lock no-error.
           if avail xCustomer then 
              lcAgrCust = lcAgrCust + " " + 
                          Func.Common:mDispCustName(BUFFER xCustomer).
        END.       

        /* invoicing customer */
        IF NOT llInvCust THEN DO:
           lcInvCust = STRING(Customer.InvCust,">>>>>>>9").
           
           FIND xCustomer where xCustomer.CustNum = Customer.InvCust 
              no-lock no-error.
           if avail xCustomer then 
              lcInvCust = lcInvCust + " " + 
                          Func.Common:mDispCustName(BUFFER xCustomer).
        END.

        CASE lcTyyppi:
        WHEN "agrusers" OR
        WHEN "invusers" THEN fr-header = " USER".
        WHEN "invcust"  THEN fr-header = " INV.CUSTOMER".
        OTHERWISE fr-header = lcChgTitle.
        END CASE.
 
        ASSIGN fr-header = fr-header + " CUSTOMER DATA "  
               Syst.Var:cfc = "kline" 
               ufkey     = TRUE.

        IF llAccess THEN
           RUN CreateReadAccess("Customer", Syst.Var:katun, Customer.CustNum, lcProgram, "CustNum" ).

        Action: 
        repeat WITH FRAME lis:
           PAUSE 0.
          
           fDispCustomer().
           
           ASSIGN 
           llDelNote[1]  = (Customer.IDelName > "")
           llDelNote[2]  = (Customer.IDelName > "")
           llDelNote[3]  = (Customer.IDelCOName > "")
           llDelNote[4]  = (Customer.IDelAddr > "")
           llDelNote[5]  = (Customer.IDelZipCode > "")
           llDelNote[6]  = (Customer.IDelPost > "")
           llDelNote[7]  = (Customer.IDelCountry > "")

           Syst.Var:ufk      = 0 
           Syst.Var:ufk[1]   = 7 WHEN lcRight =  "RW"
           Syst.Var:ufk[1]   = 0 WHEN lcRight NE "RW"
           Syst.Var:ufk[2]   = 0 
           Syst.Var:ufk[3]   = 0  
           Syst.Var:ufk[4]   = 0 
           Syst.Var:ufk[5]   = 1096  
           Syst.Var:ufk[6]   = 130  
           Syst.Var:ufk[7]   = 1883
           Syst.Var:ufk[8]   = 8 
           Syst.Var:ehto     = 0 
           ufkey    = TRUE
           liF2Type = 0
           liF3Type = 0.
           
           IF liMainCust = 0 AND iiCustNum = 0 THEN DO:

              /* browse inv.customers f3 and users f4 */ 
              IF llAgrCust THEN ASSIGN Syst.Var:ufk[3]   = 2349
                                       liF3Type = 1.
              IF llAgrCust OR llOthInvCust THEN Syst.Var:ufk[4] = 2351.
             
              /* view agreement customer */
              IF NOT llAgrCust THEN ASSIGN 
                 Syst.Var:ufk[2]   = 1025
                 liF2Type = 1.
              
              /* view inv. customer */
              IF NOT llInvCust THEN DO:
                 IF NOT llAgrCust THEN ASSIGN
                    Syst.Var:ufk[3]   = 1026
                    liF3Type = 2.
                 ELSE ASSIGN 
                    Syst.Var:ufk[2]   = 1026
                    liF2Type = 2. 
              END. 
           END.
           
           /* no financial data if only user */
           IF NOT llAgrCust AND NOT llOthInvCust THEN Syst.Var:ufk[6] = 0.
           
           PAUSE 0.
           DISPLAY llDelNote WITH FRAME lis.
           
           RUN Syst/ufkey.p.

           IF Syst.Var:toimi = 8 THEN DO:
              if (Customer.CustName = "" AND Customer.CustIDType NE "CIF") OR
                 Customer.InvGroup = ""      OR
                 Customer.Language = 0 
              THEN DO:
                 MESSAGE 
                 "Customer data is incomplete - " SKIP
                 "Choose F1 and complete the missing fields"
                 VIEW-AS ALERT-BOX ERROR.

                 NEXT Action.
              END.   

              LEAVE Action.
           END.   

           /* customer info */ 
           else IF Syst.Var:toimi = 7 THEN DO:
        
              RUN Mc/commontt.p(Customer.CustNum). 
        
           END.


           ELSE IF Syst.Var:toimi = 1 AND lcRight = "RW" THEN DO:    

              Syst.Var:ehto = 9. RUN Syst/ufkey.p.

              /* UPDATE Customer record */
              RUN local-update-customer.
              
              FIND CURRENT Customer NO-LOCK.
              NEXT CHG /*Action*/.
           END.  /* Syst.Var:toimi = 1 */

           /* view agr. customer */
           ELSE IF Syst.Var:toimi = 2 AND liF2Type = 1 THEN DO:
              
              IF Customer.CustNum = Customer.AgrCust THEN DO:
                 MESSAGE "Agreement customer is the same"
                 VIEW-AS ALERT-BOX INFORMATION.
                 NEXT.
              END.

              RUN Mc/nnasse.p (Customer.AgrCust,
                          "¤ AGR.CUSTOMER").
           END. 
            
           /* browse invoicing customers */
           ELSE IF Syst.Var:toimi = 3 AND liF3Type = 1 THEN DO:
              IF NOT llAgrCust THEN DO:
                 MESSAGE "This customer is not an agreement customer" 
                 VIEW-AS ALERT-BOX.
                 NEXT.
              END.
 
              RUN Mc/nnasse.p (Customer.CustNum,
                          "InvCust").
           END.

           /* view inv. customer */
           ELSE IF (Syst.Var:toimi = 3 AND liF3Type = 2) OR
                   (Syst.Var:toimi = 2 AND liF2Type = 2) 
           THEN DO:
              IF Customer.CustNum = Customer.InvCust THEN DO:
                 MESSAGE "Invoicing customer is the same"
                 VIEW-AS ALERT-BOX INFORMATION.
                 NEXT.
              END.

              RUN Mc/nnasse.p (Customer.InvCust,
                          "¤ INV.CUSTOMER").
           END. 
           
           /* browse users */
           ELSE IF Syst.Var:toimi = 4 THEN DO:
              IF NOT llAgrCust AND NOT llOthInvCust
              THEN DO:
                 MESSAGE "This customer is an user" VIEW-AS ALERT-BOX.
                 NEXT loop.
              END.
        
              liMainCust = Customer.CustNum.
         
              IF llAgrCust
              THEN lcTyyppi = "agrusers".
              ELSE lcTyyppi = "invusers".  
        
              RUN Mc/nnasse.p (liMainCust,
                          lcTyyppi).
                 
              ASSIGN liMainCust = 0
                     lcTyyppi   = "brand".
           END.
            
           /* contact data */
           ELSE IF Syst.Var:toimi = 5 THEN DO: 
              RUN Mc/custcont.p(Customer.CustNum).
           END.
           
           ELSE IF Syst.Var:toimi = 6 THEN DO: /* Financial Data */

              RUN local-update-fin.
              FIND CURRENT Customer NO-LOCK.
              NEXT Action.
           END.   

        END. /* Action */ 
         
        HIDE FRAME lis no-pause.

        RUN local-disp-row.
        FIND CURRENT Customer NO-LOCK.
        LEAVE.
        
     END. /* Change */

     else if lookup(Syst.Var:nap,"6,f6") > 0 THEN 
         MESSAGE "This function is not allowed." VIEW-AS ALERT-BOX.
     
     else if lookup(Syst.Var:nap,"home,h") > 0 THEN DO:

        RUN pFindFirst.
        ASSIGN memory = recid(Customer) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"end,e") > 0 THEN DO : /* LAST record */

        RUN pFindLast.

        ASSIGN memory = recid(Customer) must-print = TRUE.

        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"8,f8") > 0 THEN DO:
        
        LEAVE LOOP.
     END.  
     
     IF iiCustnum > 0 THEN LEAVE.
     
  END.  /* BROWSE */

  IF iiCustnum > 0 THEN LEAVE.

END.  /* LOOP */

HIDE FRAME sel no-pause.
Syst.Var:si-recid = xrecid.
fCleanEventObjects(). 

PROCEDURE local-update-customer:

   CUST_UPDATE:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
      ASSIGN ldOrigMSLimit    = pdMobSubLimit
             ldOrigMSActLimit = pdMobSubActLimit.
      
      fCountry(Customer.Country).
      fCustCat(Customer.Category).
      fLanguage(Customer.Language).
      fRegion(Customer.Region).
      fNationality(Customer.Nationality).
      pdMobSubLimit = fGetMobSubLimit(Customer.Custnum, 
                                      Customer.Category, 
                               OUTPUT llMSLimitIsDefault).
      pdMobSubActLimit = fGetMobSubActLimit(Customer.Custnum, 
                                            Customer.Category, 
                                            OUTPUT llMSActLimitIsDefault).
      
      fDispCustomer().
      
      PROMPT 
      Customer.CustIDType 
      Customer.OrgId 
      Customer.BirthDay
      Customer.Nationality
      Customer.HonTitle
      Customer.FirstName
      Customer.CustName 
      Customer.SurName2
      Customer.CompanyName
      Customer.Language
      Customer.Category 
      pdMobSubLimit
      pdMobSubActLimit
      Customer.DataProtected 
      WITH FRAME lis
      {Mc/nnasse.i}


      /* cross reference checks */  
      IF Customer.CustNum > 1000 THEN DO:

         FIND CustCat WHERE 
              CustCat.Brand    = Syst.Var:gcBrand AND
              CustCat.Category = INPUT Customer.Category NO-LOCK NO-ERROR.
         IF NOT AVAILABLE CustCat THEN DO:
             MESSAGE "Unknown category"
            VIEW-AS ALERT-BOX ERROR.
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END.
         ELSE IF 
            LOOKUP(INPUT Customer.CustIDType,CustCat.CustIDType) = 0 
         THEN DO:
            MESSAGE "There is a conflict between chosen ID type and category"
            VIEW-AS ALERT-BOX ERROR.
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END.
         
         IF NOT fChkCustID(INPUT INPUT Customer.CustIDType,
                           INPUT INPUT Customer.OrgId)
         THEN DO:
            MESSAGE "Invalid customer ID or ID Type"
            VIEW-AS ALERT-BOX ERROR.
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END.
         
         FIND FIRST xCustomer WHERE
            xCustomer.Brand = Syst.Var:gcBrand AND
            xCustomer.OrgID = INPUT Customer.OrgId AND
            xCustomer.CustIDType = INPUT Customer.CustIDType AND
            xCustomer.Roles NE "inactive" AND
            ROWID(xCustomer) NE ROWID(Customer)
         NO-LOCK NO-ERROR.

         IF AVAIL xCustomer THEN DO:
            RELEASE xCustomer.
            MESSAGE "Other customer exists with same ID"
            VIEW-AS ALERT-BOX ERROR.
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END.
      
         IF INPUT Customer.FirstName + 
            INPUT Customer.CustName + 
            INPUT Customer.SurName2 > "" AND
            INPUT Customer.CompanyName > "" AND 
            INPUT Customer.Profession = "" AND
            INPUT Customer.CustIdType NE "CIF" THEN DO:
            MESSAGE "You can't give both company name and consumer name"
            VIEW-AS ALERT-BOX ERROR.
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END.
      
         /* company vrs. consumer */  
         IF (INPUT Customer.CustIDType = "CIF"   AND 
              INPUT Customer.CompanyName = "" )       OR
            (LOOKUP(INPUT Customer.CustIDType,"CIF,N/A") = 0 AND 
             INPUT Customer.CompanyName > "" AND
             INPUT Customer.Profession = "")
         THEN DO:
             MESSAGE "There is a conflict between ID type and given names"
             VIEW-AS ALERT-BOX ERROR.
             UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END. 

         IF LOOKUP(INPUT Customer.CustIDType,"N/A,CIF") = 0 AND 
            (INPUT Customer.HonTitle  = ""  OR
             INPUT Customer.FirstName = ""  OR
             INPUT Customer.CustName  = "")
         THEN DO:
            MESSAGE "Name data is missing"
            VIEW-AS ALERT-BOX ERROR.
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END.
         
         /* country vrs. id type */
         IF (LOOKUP(INPUT Customer.CustIDType,"NIE,NIF,CIF,N/A") > 0 AND 
              Customer.Country NE lcDefCountry) THEN DO:
            
            MESSAGE
               "There is a conflict between ID type and country"
            VIEW-AS ALERT-BOX ERROR.
         
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.

         END.

         FIND CURRENT Customer EXCLUSIVE-LOCK.
         
         IF CURRENT-CHANGED Customer THEN DO:
            
            FIND CURRENT Customer NO-LOCK.
            
            MESSAGE 
               "This record has been changed elsewhere while updating" 
            VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

            UNDO CUST_UPDATE, LEAVE CUST_UPDATE.

         END. 
         ELSE DO: 
            
            IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhCustomer ).
            
            ASSIGN
               Customer.CustIDType 
               Customer.OrgId 
               Customer.BirthDay
               Customer.Nationality
               Customer.HonTitle
               Customer.FirstName
               Customer.CustName 
               Customer.SurName2
               Customer.CompanyName
               Customer.Language
               Customer.Category
               Customer.DataProtected.
            
            /* Customer info */
            IF llDoEvent THEN 
               RUN StarEventMakeModifyEventWithMemo(lhCustomer, 
                                                    {&STAR_EVENT_USER}, 
                                                    lcMemo).
        
         END.

         ASSIGN pdMobSubLimit
                pdMobSubActLimit.

         /* Activation limit should not be less than subscription limit */
         IF pdMobSubActLimit < pdMobSubLimit THEN DO:
            MESSAGE "Activation limit can not be less than Mobsub limit !".
            UNDO CUST_UPDATE, NEXT CUST_UPDATE.
         END. /* IF pdMobSubActLimit < pdMobSubLimit THEN */

         IF pdMobSubLimit NE ldOrigMSLimit THEN DO:
            
            llCleanFLimitReqEventLog = FALSE.
            
            fGetLimit (Customer.Custnum, 0, {&LIMIT_TYPE_SUBQTY}, 0, 0, TODAY).

            IF AVAIL Limit THEN DO:
               IF Limit.LimitAmt NE pdMobsubLimit THEN
                  fSetLimit(ROWID(Limit), pdMobsubLimit, FALSE, TODAY, 12/31/2049).
            END.
            ELSE DO:
               fCreateLimit(Customer.Custnum,
                            0,
                            {&LIMIT_TYPE_SUBQTY},
                            pdMobsubLimit,
                            0,
                            0,
                            TODAY,
                            12/31/2049).
            END.
            
            llMSLimitIsDefault = FALSE.
            llCleanFLimitReqEventLog = TRUE. 
         END.

         IF pdMobSubActLimit NE ldOrigMSActLimit THEN DO:
            
            llCleanFLimitReqEventLog = FALSE.
            
            fGetLimit (Customer.Custnum, 0, {&LIMIT_TYPE_SUBACTQTY}, 0, 0, TODAY).

            IF AVAIL Limit THEN DO:
               IF Limit.LimitAmt NE pdMobsubActLimit THEN
                  fSetLimit(ROWID(Limit), pdMobsubActLimit, FALSE, TODAY, 12/31/2049).
            END.
            ELSE DO:
               fCreateLimit(Customer.Custnum,
                            0,
                            {&LIMIT_TYPE_SUBACTQTY},
                            pdMobsubActLimit,
                            0,
                            0,
                            TODAY,
                            12/31/2049).
            END.
            
            llMSActLimitIsDefault = FALSE.
            llCleanFLimitReqEventLog = TRUE. 
         END.
        
      END.
     
      LEAVE.
   END.   
  
END PROCEDURE.

PROCEDURE local-update-fin:

   DEF VAR liRequest    AS INT  NO-UNDO.
   DEF VAR lcResult     AS CHAR NO-UNDO.
   DEF VAR llUpdateDelType AS LOG NO-UNDO. 

   IF llCLGroup THEN DO:
      FIND FIRST addcustlimit WHERE 
                 addcustlimit.custNum = Customer.CustNum AND
                 addcustlimit.dto   >= today          NO-LOCK NO-ERROR.

      IF AVAIL addcustlimit THEN DO:
         BELL.
         MESSAGE
         "Max Credit of this customer has been temporarily changed!" SKIP
         "Temporary limit is " addcustlimit.amount   "and it is valid till "
         addcustlimit.dto "."                                        SKIP
         VIEW-AS ALERT-BOX TITLE "TEMPORARY CUSTOMER LIMIT CHANGE".
      END.
   END.

   lcCustName = Func.Common:mDispCustName(BUFFER Customer).
 
   IF lcRight = "RW" then
   ACTION: 
   repeat with frame fina:

      CLEAR FRAME fina NO-PAUSE.

      lcDType = Func.Common:mTMSCodeName("Invoice",
                                 "DelType",
                                 STRING(Customer.DelType)).
      lcCType = Func.Common:mTMSCodeName("Customer",
                                 "ChargeType",
                                 STRING(Customer.ChargeType)).

      lcSType = Func.Common:mTMSCodeName("Invoice",
                                 "SpecDel",
                                 STRING(Customer.SpecDel)).
      
      lcInvTargetRule = Func.Common:mTMSCodeName("Customer",
                                 "InvoiceTargetRule",
                                 STRING(Customer.InvoiceTargetRule)).

      fDispVATUsage(Customer.VATUsage).

      DISP 
         Customer.CustNum 
         lcCustName 
         Customer.Currency
         Customer.VATUsage
         Customer.VATIncl 
         Customer.Currency
         Customer.PaymTerm
         Customer.ClaimPerm
         Customer.InterestPerm
         Customer.DelType
         Customer.ChargeType
         Customer.CreditLimit
         Customer.InvoiceTargetRule lcInvTargetRule
         lcCType 
         lcDType 
         lcCutLine
      WITH FRAME fina.

      /* bank account from direct debit authorization, 
         if no authorization */
      RUN Ar/nnsvte.p (Customer.CustNum,
                  TODAY, 
                  OUTPUT lcBankAcc).
      IF lcBankAcc NE "" 
      THEN ASSIGN llDDBank  = TRUE
                  lcBankAcc = fBankData2Acc(lcBankAcc).
      ELSE ASSIGN llDDBank = FALSE
                  lcBankAcc = Customer.BankAcc. 

      ASSIGN lcBankName1  = ""
             lcBankAddr   = ""
             lcBankPost   = "". 
      IF lcBankAcc > "" THEN DO:
   
         FIND FIRST Bank WHERE
                    Bank.Brand      = Syst.Var:gcBrand AND
                    Bank.BankID     = SUBSTRING(lcBankAcc,5,4) AND
                    Bank.BankOffice = SUBSTRING(lcBankAcc,9,4) NO-LOCK NO-ERROR.
         IF AVAILABLE Bank THEN ASSIGN 
            lcBankName1 = Bank.Name
            lcBankAddr  = Bank.Address
            lcBankPost  = Bank.ZipCode + " " + Bank.City.
      END.

      DISPLAY lcBankAcc 
              lcBankName1
              lcBankAddr
              lcBankPost
      WITH FRAME fina. 

      ASSIGN
      Syst.Var:ufk = 0
      Syst.Var:ufk[1] = 7
      Syst.Var:ufk[2] = 1717
      Syst.Var:ufk[3] = 1122
      Syst.Var:ufk[5] = 1164
      Syst.Var:ufk[8] = 8
      Syst.Var:ehto = 0
      ufkey = true.
      RUN Syst/ufkey.p.
                                                             
      IF Syst.Var:toimi = 8 then do:
         hide frame fina.
         LEAVE.
      end.

      ELSE IF Syst.Var:toimi = 1 AND lcRight = "RW" THEN DO:
      BILLDATA_CHG:
      REPEAT WITH FRAME fina ON ENDKEY UNDO, LEAVE:

         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
         
         PROMPT
         lcBankAcc WHEN NOT llDDBank
         Customer.ClaimPerm
         Customer.InterestPerm
         Customer.InvoiceTargetRule
         Customer.ChargeType
         Customer.PaymTerm WHEN Customer.CustNum < 1000
         Customer.DelType
         Customer.Currency WHEN Customer.CustNum < 1000
         WITH FRAME fina EDITING:

            READKEY.

            /* For preventing field level validation skipping */
            IF LOOKUP(KEYLABEL(lastkey),"END,HOME") > 0 THEN NEXT.

            IF KEYLABEL(lastkey) = "F9" AND 
               LOOKUP(FRAME-FIELD,"ChargeType,DelType,SpecDel,VatUsage,InvoiceTargetRule") > 0
            THEN DO WITH FRAME fina:
               
               IF FRAME-FIELD = "InvoiceTargetRule" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Customer",     /* TableName*/
                                       "InvoiceTargetRule", /* FieldName */
                                       "Billing",      /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY INTEGER(lcCode) ;& Customer.InvoiceTargetRule
                             lcInvTargetRule WITH FRAME fina.
                  END.

               END.

               IF FRAME-FIELD = "ChargeType" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Customer",     /* TableName*/
                                       "ChargeType",       /* FieldName */
                                       "AccRec",      /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY INTEGER(lcCode) ;& Customer.ChargeType
                             lcCType WITH FRAME fina.
                  END.

               END.

               ELSE IF FRAME-FIELD = "DelType" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",     /* TableName*/
                                       "DelType",       /* FieldName */
                                       "Billing",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY INTEGER(lcCode) ;& Customer.DelType
                             lcDType WITH FRAME fina.
                  END.

               END.
                
               
               ELSE IF FRAME-FIELD = "VATUsage" THEN DO:
              
                  RUN Help/h-tmscodes.p(INPUT "Invoice",     /* TableName*/
                                       "VatUsage",       /* FieldName */
                                       "Billing",     /* GroupCode */
                                 OUTPUT lcCode).
             
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY INTEGER(lcCode) ;& Customer.VATUsage 
                     WITH FRAME fina.
                  END.
                  
               END.
     
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF lookup(keylabel(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME fina:
               PAUSE 0.

               IF FRAME-FIELD = "VATUsage" THEN DO:
                  IF NOT fDispVatUsage(INPUT INPUT Customer.VATUsage) THEN DO:
                     MESSAGE "Unknown VAT usage type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "Currency" THEN 
               DO:
                  FIND FIRST Currency WHERE currency.currency = 
                  INPUT FRAME FINA Customer.currency
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE Currency THEN
                  DO:
                     BELL.
                     MESSAGE "Unknown Currency !".
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "ChargeType" THEN DO:

                  /* check that cType and dType -combination is valid */
                  IF NOT fChkCType(INPUT INPUT Customer.ChargeType,
                                   INPUT INPUT Customer.DelType,
                                   TRUE)
                  THEN NEXT. 

                  lcCType = 
                     Func.Common:mTMSCodeName("Customer",
                                      "ChargeType",
                                      STRING(INPUT Customer.ChargeType)).
                  DISPLAY lcCType.
             
                  IF lcCType = "" THEN DO:
                     MESSAGE "Unknown charge type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.   
                  
               END.

               ELSE IF FRAME-FIELD = "DelType" THEN DO:

                  lcDType = 
                     Func.Common:mTMSCodeName("Invoice",
                                      "DelType",
                                      STRING(INPUT Customer.DelType)).
                  DISPLAY lcDType.
 
                  IF lcDType = "" THEN DO:
                     MESSAGE "Unknown delivery type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.   

                  IF Customer.DelType NE INPUT Customer.Deltype AND 
                     LOOKUP(STRING(INPUT Customer.Deltype)
                     ,SUBST("&1,&2,&3",
                        {&INV_DEL_TYPE_EMAIL_PENDING},
                        {&INV_DEL_TYPE_FUSION_EMAIL_PENDING},
                        {&INV_DEL_TYPE_FUSION_EMAIL})) > 0
                     THEN DO:
                     MESSAGE "Incorrect delivery type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.

                  END.
               END.
               
               ELSE IF FRAME-FIELD = "InvoiceTargetRule" THEN DO:

                  lcInvTargetRule = 
                     Func.Common:mTMSCodeName("Customer",
                                      "InvoiceTargetRule",
                                      STRING(INPUT Customer.InvoiceTargetRule)).
                  DISPLAY lcInvTargetRule.
 
                  IF lcInvTargetRule = "" THEN DO:
                     MESSAGE "Unknown invoice target rule"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "lcBankAcc" THEN DO:
                  IF INPUT lcBankAcc > "" THEN DO:
                     IF NOT fCheckBankAcc(INPUT INPUT lcBankAcc) THEN DO:
                        MESSAGE "Given bank account is not valid."
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
                  ELSE IF NOT fChkBankAccChange(INPUT Customer.CustNum)
                  THEN DO:
                     MESSAGE "Bank account can not remain empty"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
            END.
            APPLY LASTKEY.
         END.   
            
         FIND CURRENT Customer NO-LOCK.

         IF CURRENT-CHANGED Customer THEN DO:
            
            MESSAGE 
               "This record has been changed elsewhere while updating" 
            VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

            UNDO BILLDATA_CHG, LEAVE BILLDATA_CHG.

         END. 
         ELSE DO: 
         
            FIND CURRENT Customer EXCLUSIVE-LOCK.
            
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer). 

            IF INPUT Customer.DelType <> Customer.DelType THEN DO:
               llUpdateDelType = TRUE.
               IF INPUT Customer.DelType = {&INV_DEL_TYPE_EMAIL} THEN DO:
                  IF Customer.Email = "" THEN DO:
                      MESSAGE "Invoice delivery type to Email can not be " +
                              "changed since customer does not have email address"
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                  END. /* IF Customer.Email = "" THEN DO: */

                  liRequest = fEmailInvoiceRequest(
                                       INPUT Func.Common:mMakeTS(),
                                       INPUT TODAY,
                                       INPUT Syst.Var:katun,
                                       INPUT 0,
                                       INPUT "",
                                       INPUT Customer.CustNum,
                                       INPUT {&REQUEST_SOURCE_MANUAL_TMS},
                                       INPUT Customer.Email,
                                       INPUT 0, /*orderid*/
                                       OUTPUT lcResult).
                  IF liRequest = 0 THEN DO:
                     IF lcResult = "Customer already has an active request" THEN .
                     ELSE DO:
                        MESSAGE "Invoice delivery type to Email " +
                                "can not be changed: " + lcResult
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END. /* ELSE DO: */
                  END. /* IF liRequest = 0 THEN DO: */
                  
                  /* If Email already validated then mark DelType EMAIL */
                  IF liRequest NE 1 THEN ASSIGN
                     Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}
                     llUpdateDelType = FALSE.
               END. /* IF INPUT Customer.DelType = {&INV_DEL_TYPE_EMAIL} */
               ELSE IF (INPUT Customer.DelType = {&INV_DEL_TYPE_SMS} OR
                        INPUT Customer.DelType = {&INV_DEL_TYPE_ESI}) AND
                    BUFFER-TENANT-NAME(Customer) EQ {&TENANT_MASMOVIL} THEN DO:
                  MESSAGE "SMS Invoice not allowed for MasMovil"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               ELSE DO:
                  /* Cancel Ongoing Email Activation Request */
                  FIND FIRST InvoiceTargetGroup WHERE
                             InvoiceTargetGroup.CustNum = Customer.CustNum AND
                             InvoiceTargetGroup.ToDate >= TODAY AND
                            (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                             InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
                       NO-LOCK NO-ERROR.
                  IF NOT AVAIL InvoiceTargetGroup AND
                     fPendingEmailActRequest(Customer.Custnum) THEN
                     fCancelPendingEmailActRequest(Customer.Custnum,
                                         INPUT "Invoice Delivery Type is " +
                                         "changed to " +
                                         STRING(INPUT Customer.DelType)).
               END. /* ELSE DO: */
            
            END. /* IF INPUT Customer.DelType <> Customer.DelType */

            ASSIGN
               lcBankAcc WHEN NOT llDDBank
               Customer.ClaimPerm
               Customer.InterestPerm
               Customer.InvoiceTargetRule
               Customer.ChargeType
               Customer.PaymTerm WHEN Customer.CustNum < 1000
               Customer.DelType WHEN llUpdateDeltype
               Customer.Currency WHEN Customer.CustNum < 1000.

         
            IF NOT llDDBank THEN DO:

               CREATE Memo.
               ASSIGN
                  Memo.CreStamp  = Func.Common:mMakeTS()
                  Memo.Brand     = Syst.Var:gcBrand
                  Memo.Custnum   = Customer.CustNum
                  Memo.HostTable = "Customer"
                  Memo.KeyValue  = STRING(Customer.CustNum)
                  Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
                  Memo.CreUser   = Syst.Var:katun
                  Memo.MemoTitle = "Cambio de cuenta"
                  Memo.MemoText  = "Solicitado por el cliente: Nº de " +
                                   "cuenta: " + Customer.BankAcc + " --> " +
                                   IF lcBankAcc > "" THEN lcBankAcc ELSE
                                   "blank".

               Customer.BankAcc = lcBankAcc.


            END.
            /* Billing data */
            IF llDoEvent THEN 
               RUN StarEventMakeModifyEventWithMemo(lhCustomer, 
                                                    {&STAR_EVENT_USER}, 
                                                    lcMemo).
        
         END.
         
         FIND CURRENT Customer NO-LOCK.
         LEAVE.
      END.
      END.   
   
      ELSE IF Syst.Var:toimi = 2 THEN DO:
         RUN Ar/custbal.p (Customer.CustNum).
      END.
      
      ELSE IF Syst.Var:toimi = 3 AND lcRight = "RW" THEN DO:
         
         REPEAT WITH FRAME fina ON ENDKEY UNDO, LEAVE:
            
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
            
            PROMPT Customer.CreditLimit.
         
            FIND CURRENT Customer EXCLUSIVE-LOCK.
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer). 
            ASSIGN Customer.CreditLimit.
            /* Billing data */
            IF llDoEvent THEN 
               RUN StarEventMakeModifyEventWithMemo(lhCustomer, 
                                                    {&STAR_EVENT_USER}, 
                                                    lcMemo).
            FIND CURRENT Customer NO-LOCK.

            LEAVE.
         END.
      
      END.

      ELSE IF Syst.Var:toimi = 5 THEN DO:                 
         RUN Mm/pclist.p("Customer",Customer.CustNum).
      END.                                        
   END. 
   ELSE PAUSE.

   HIDE FRAME fina no-pause.

END PROCEDURE.

PROCEDURE copy-values:

   FIND LAST xCustomer where
             xCustomer.SearchName = Customer.SearchName AND
             xCustomer.CustName begins "DUMMY"
   no-lock no-error.

   DISP
      xCustomer.CustNum
      xCustomer.CustName
   WITH FRAME copy.

   ok = TRUE.
   UPDATE ok WITH FRAME copy.

   IF ok THEN DO:

      ASSIGN
         Customer.InvGroup   = xCustomer.InvGroup
         Customer.Size       = xCustomer.Size
         Customer.Category   = xCustomer.Category
         Customer.SearchName = xCustomer.SearchName
         Customer.ConnType   = xCustomer.ConnType
         Customer.RepCodes   = xCustomer.RepCodes.

      DISP   
         Customer.InvGroup
         Customer.Category
      WITH FRAME lis.

   END.

   HIDE FRAME copy.

END PROCEDURE.

PROCEDURE local-disp-row:

   memochr = CAN-FIND(FIRST memo WHERE
                            memo.Brand     = Customer.Brand AND 
                            memo.HostTable = "Customer" AND
                            memo.KeyValue  = STRING(Customer.CustNum)).

   fLocalCustRoles().
   
   ASSIGN lcRoles    = STRING(llAgrCust,"X/") + " " +
                       STRING(llOthInvCust,"x/") + " " +
                       STRING(llUser,"x/")
          lcCustName = Func.Common:mDispCustName(BUFFER Customer).
                    
   
   DISPLAY Customer.CustNum 
           lcCustName
           Customer.ZipCode 
           Customer.PostOffice 
           Customer.OrgId 
           lcRoles
           memochr 
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE pFindFirst.
   IF      order = 1 THEN DO: {Mc/custfind.i FIRST CustNum
                                          "AND Customer.CustNum > 1000"}. END.
   ELSE IF order = 2 THEN DO: {Mc/custfind.i FIRST CustName
                                          "AND Customer.CustNum > 1000"}. END.
   ELSE IF order = 3 THEN DO: {Mc/custfind.i FIRST ZipCode
                                          "AND Customer.CustNum > 1000"}. END.
   ELSE IF order = 4 THEN DO: {Mc/custfind.i FIRST OrgId
                                          "AND Customer.CustNum > 1000"}. END.
   ELSE IF order = 5 THEN DO: {Mc/custfind.i FIRST SearchName
                                          "AND Customer.CustNum > 1000"}. END.
END PROCEDURE. 

PROCEDURE pFindLast.
   IF      order = 1 THEN DO: {Mc/custfind.i LAST CustNum}.    END.
   ELSE IF order = 2 THEN DO: {Mc/custfind.i LAST CustName}.   END.
   ELSE IF order = 3 THEN DO: {Mc/custfind.i LAST ZipCode}.    END.
   ELSE IF order = 4 THEN DO: {Mc/custfind.i LAST OrgId}.      END.
   ELSE IF order = 5 THEN DO: {Mc/custfind.i LAST SearchName}. END.
END PROCEDURE.

PROCEDURE pFindNext.
   IF      order = 1 THEN DO: {Mc/custfind.i NEXT CustNum}.    END.
   ELSE IF order = 2 THEN DO: {Mc/custfind.i NEXT CustName}.   END.
   ELSE IF order = 3 THEN DO: {Mc/custfind.i NEXT ZipCode}.    END.
   ELSE IF order = 4 THEN DO: {Mc/custfind.i NEXT OrgId}.      END.
   ELSE IF order = 5 THEN DO: {Mc/custfind.i NEXT SearchName}. END.
END PROCEDURE.

PROCEDURE pFindPrev.
   IF      order = 1 THEN DO: {Mc/custfind.i PREV CustNum}.    END.
   ELSE IF order = 2 THEN DO: {Mc/custfind.i PREV CustName}.   END.
   ELSE IF order = 3 THEN DO: {Mc/custfind.i PREV ZipCode}.    END.
   ELSE IF order = 4 THEN DO: {Mc/custfind.i PREV OrgId}.      END.
   ELSE IF order = 5 THEN DO: {Mc/custfind.i PREV SearchName}. END.
END PROCEDURE.




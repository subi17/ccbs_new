/* nnasse.frm       

   changed:      09.09.02/aam voldisc removed 
                 16.09.02/aam PriceList and RateCust removed
                 09.10.02/jr  Country name  
                 18.10.02/aam InvCode added 
                 20.11.02 lp  AccGrp and accname added
                 28.02.03/aam DirMark and CCLang added 
                 25.03.03/aam RateCust into use again
                 01.10.03/aam Salesman and Reseller removed
                 16.01.04/aam AgrCust added, AccGrp removed
                 09.02.04/aam show stars (*) if delivery address exists
                 31.05.04/aam SMSNumber
                 07.12.05/aam -> trel2
                 14.11.06/aam yoigo layout
*/   

  Customer.CustIDType  
     LABEL "ID Type ...."
  Customer.CustNum   AT 46
     LABEL "Cust. Number" 
     SKIP

  Customer.OrgId 
     LABEL "Customer ID "  
     help "Customer ID"     
  Customer.Category AT 46
     LABEL "Category ..." 
     dkatnimi FORMAT "X(14)" NO-LABEL 
     SKIP

  Customer.BirthDay 
     LABEL "Birthday ..."
     FORMAT "99-99-9999"
  Customer.InvGroup AT 46
     LABEL "Invoice Grp." 
     SKIP

  Customer.Nationality 
     LABEL "Nationality " 
     FORMAT "X(2)" 
  lcNationality
     NO-LABEL 
     FORMAT "X(12)" 
  Customer.CreDate AT 46
     LABEL "Created ...." 
     SKIP
 
  Customer.HonTitle 
     LABEL "Title ......"
     HELP  "Title" 
     FORMAT "X(4)"

     SKIP
  
  "Firstname ..:"
     SPACE(0)
     llDelNote[2] NO-LABEL
     SPACE(0)
  Customer.FirstName NO-LABEL   
  
  pdMobSubLimit AT 46 
     Label "Mobsub limit"
     FORMAT ">>9"
     HELP "Active subscription limit per customer"
   llMSLimitIsDefault AT 64
     NO-LABEL
     FORMAT "Default/Custom" 
     SKIP


  "Surname 1 ..:" 
     SPACE(0) 
     llDelNote[1] NO-LABEL
     SPACE(0)
  Customer.CustName NO-LABEL 
     HELP "First surname for private customer" 

  pdMobSubActLimit AT 46 
     Label "Activation limit"
     FORMAT ">>9"
     HELP "Mobsub activation limit per customer"
   llMSActLimitIsDefault AT 68
     NO-LABEL
     FORMAT "Default/Custom" 
     SKIP

  "Surname 2 ..:" 
  Customer.SurName2 NO-LABEL 
     HELP "Second surname for private customer" 
  Customer.DataProtected AT 46 LABEL "Data protection required" FORMAT "Yes/No"
     SKIP

  Customer.CompanyName
     LABEL "Company Name"
  Customer.Profession AT 46 LABEL "Profession" FORMAT "X(2)"
     lcProfession NO-LABEL FORMAT "X(18)"
  SKIP   
    
 
  "C/O ........:"
     SPACE(0) 
     llDelNote[3] NO-LABEL
     SPACE(0)
  lcCustCOName NO-LABEL 
     SKIP

  "Address ....:"
     SPACE(0) 
     llDelNote[4] NO-LABEL
     SPACE(0)
  lcCustAddress  
     NO-LABEL 
     FORMAT "X(30)"
   Customer.AuthCustIdType AT 46
     LABEL "Authorized Type"
     SKIP

  "Zip Code ...:"
     SPACE(0) 
     llDelNote[5] NO-LABEL
     SPACE(0)
  lcCustZipCode   NO-LABEL 
  Customer.AuthCustId AT 46
     LABEL "Authorized ID"
     SKIP

  "City ...... :"
     SPACE(0) 
     llDelNote[6] NO-LABEL
     SPACE(0)
     lcCustPostOffice  format "x(62)" 
     NO-LABEL  
    
     SKIP
  lcCustRegion 
     LABEL "Region ....." 
     FORMAT "X(3)"
  lcRegion 
     NO-LABEL 
     FORMAT "X(12)" 
  llAgrCust AT 46 
     LABEL "Agr.Customer" 
     FORMAT "Yes/No"
  lcAgrCust 
     NO-LABEL
     FORMAT "X(13)" 
     SKIP    
   "Country ....:"  
     SPACE(0) 
     llDelNote[7] NO-LABEL
     SPACE(0)
  lcCustCountry   NO-LABEL 
     FORMAT "x(3)"
  lcCountry
     NO-LABEL 
     FORMAT "X(20)" 
  llInvCust AT 46
     LABEL "Inv.Customer"
     FORMAT "Yes/No"
  lcInvCust 
     NO-LABEL
     FORMAT "X(13)" 
     SKIP
 

  Customer.Language 
     LABEL "Language ..."    
     lcLang NO-LABEL 
  llOthInvCust AT 46
     LABEL "Acts As Inv." 
     FORMAT "Yes/No" 
  SKIP

  llAddressValidated 
     LABEL "Addr.valid.." FORMAT "Yes/No"
  llUser  AT 46 
     LABEL "User ......."
     FORMAT "Yes/No"  
  SKIP

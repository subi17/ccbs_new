/* --------------------------------------------------------------------------
  MODULE .......: NNTUYP.P
  FUNCTION .....: Maintain BillCode data 
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 12-08-96
  MODIFIED .....: 20.07.01 kl: RepText instead of tunimi
                  13.02.02 jp: BillCode FORMAT x(16)
                  16.05.02 aam InvSect AND ISOrder added 
                  16.05.02/tk Event logging added
                  20.05.02/tk invoice texts
                  22.07.02/tk show full page on "end"
                  29.07.02 lp F2 -> Find Name(was find name)
                  23.08.02/tk use TB2AccNum for epl-form
                              -> dont update cost accounts
                              update VatCode
                  05.09.02 jp validation
                  26.02.03 tk tokens
                  28.02.03 aam epl-form removed 
                  05.09.03 aam brand
                  08.01.04 aam EUAccNum, FSAccNum, EUVatCode added,
                               sections removed
                  12.02.04/aam Accounts to 6 digits
                  28.09.04/aam AltAccNum
                  04.05.05/aam use DispMPM for "Disp MPM"
                  30.11.05/aam longer format for name
                  13.11.06/aam TaxClass instead of VatCode
                  14.11.06/aam account with 8 digits
                  21.11.06/aam SAPRid, VipAccNum,
                               translations (invlang)
                  
  Version ......: yoigo
  -------------------------------------------------------------------------- */

 RUN nntuyp_run (?,"update-mode-general").




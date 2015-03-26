from gearbox.migrations import Migration

class AddOrderCustomer(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderCustomer', area='Sta_Data_32',
                       dump_name='ordercus')
        t.column('RowType', 'integer', format='>>9', initial='0',
                 column_label='RowType',
                 help='Order customer type')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='CustNum',
                 help='Reference customer number',
                 description='Reference customer number')
        t.column('FirstName', 'character', format='x(20)', initial='',
                 label='ForeName',
                 column_label='ForeName',
                 help='Customer\'s fore/given name',
                 description='Customer\'s forename')
        t.column('SurName1', 'character', format='x(20)', initial='',
                 column_label='SurName1',
                 help='Customer\'s 1st last name',
                 description='Customer\'s 1st last name')
        t.column('Address', 'character', format='x(30)', initial='',
                 column_label='Address',
                 help='Customer\'s mailing address (street, p.o. box)')
        t.column('ZipCode', 'character', initial='',
                 label='Postal code',
                 column_label='Postcd',
                 help='Customer\'s postal code')
        t.column('PostOffice', 'character', format='x(24)', initial='',
                 label='Postal Addr.',
                 column_label='Postaddr',
                 help='Customer\'s postal address')
        t.column('ContactNum', 'character', format='x(12)', initial='',
                 column_label='ContactNum',
                 description='Contact Phone number')
        t.column('Email', 'character', format='x(60)', initial='',
                 column_label='Email',
                 help='Customer\'s Email ID')
        t.column('BirthDay', 'date', format='99.99.99',
                 column_label='BirthDay')
        t.column('Sex', 'character', initial='')
        t.column('Country', 'character', format='x(2)', initial='',
                 help='Agreement customer\'s country')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('SurName2', 'character', format='x(20)', initial='',
                 column_label='SurName2',
                 help='Customer\'s 2nd last name',
                 description='Customer\'s 2nd last name')
        t.column('CustId', 'character', format='x(12)', initial='',
                 column_label='CustId')
        t.column('CustIdType', 'character', initial='',
                 column_label='CustIdType')
        t.column('Company', 'character', format='x(20)', initial='',
                 column_label='Company',
                 help='Company name',
                 description='Company name')
        t.column('Nationality', 'character', format='x(2)', initial='',
                 column_label='Nationality')
        t.column('Region', 'character', format='x(12)', initial='',
                 column_label='Region')
        t.column('MobileNumber', 'character', format='x(10)', initial='',
                 column_label='MobileNumber')
        t.column('FixedNumber', 'character', format='x(10)', initial='',
                 column_label='FixedNumber')
        t.column('Language', 'character', format='x(2)', initial='',
                 column_label='Language')
        t.column('OperSMSMarketing', 'logical', initial='no',
                 column_label='OperSMSMarketing')
        t.column('OperEMailMarketing', 'logical', initial='no',
                 column_label='OperEMailMarketing')
        t.column('OperPostMarketing', 'logical', initial='no',
                 column_label='OperPostMarketing')
        t.column('OutEMailMarketing', 'logical', initial='no',
                 column_label='OutEMailMarketing')
        t.column('OutPostMarketing', 'logical', initial='no',
                 column_label='OutPostMarketing')
        t.column('OutSMSMarketing', 'logical', initial='no',
                 column_label='OutSMSMarketing')
        t.column('OperAllMarketing', 'logical', initial='no',
                 column_label='OperAllMarketing')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Category', 'character', format='x(4)', initial='',
                 column_label='Cat.',
                 help='Category code of agreement customer')
        t.column('PersonId', 'character', format='x(11)', initial='',
                 column_label='PersonId',
                 help='Personal Number',
                 description='Personal Number')
        t.column('CustTitle', 'character', format='x(12)', initial='',
                 label='Title',
                 column_label='Title')
        t.column('BankCode', 'character', format='x(10)', initial='',
                 column_label='BankCode')
        t.column('CustUserId', 'character', initial='',
                 label='UserId',
                 column_label='UserId')
        t.column('CustPWD', 'character', initial='',
                 label='Password',
                 column_label='Password')
        t.column('VatPercent', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Vat%',
                 column_label='Vat%')
        t.column('VatCode', 'character', format='x(4)', initial='',
                 column_label='VatCode')
        t.column('Passport', 'character', format='x(12)', initial='',
                 column_label='Passport')
        t.column('AddressCodC', 'character', initial='',
                 label='Address CodC',
                 column_label='CodC',
                 help='CodC in address validation')
        t.column('AddressCodP', 'character', initial='',
                 label='Address CodP',
                 column_label='CodP',
                 help='CodP in address validation')
        t.column('SelfEmployed', 'logical', initial='no',
                 label='Selfemployed',
                 column_label='Selfempl.')
        t.column('FoundationDate', 'date', format='99-99-99',
                 label='Foundation Date',
                 column_label='Found.Date')
        t.column('ExtInvRef', 'character', format='X(50)', initial='',
                 label='External InvRef',
                 column_label='ExtInvRef',
                 help='External Invoice Reference',
                 description='External invoice reference for corporate customers')
        t.column('SubQty', 'integer', format='>>>>9', initial='0',
                 column_label='SubQty',
                 help='Max subscription quantity',
                 description='Max subscription quantity for customer')
        t.column('DataChecked', 'logical', initial='No',
                 column_label='DataChecked',
                 help='Is the data checked',
                 description='Is the ordercustomer data checked')
        t.column('Street', 'character', format='x(30)', initial='',
                 column_label='Street')
        t.column('BuildingNum', 'character', initial='',
                 label='Building Number',
                 column_label='Building Number')
        t.column('AddressCompl', 'character', format='x(20)', initial='',
                 label='Address Complement',
                 column_label='Address Complement')
        t.column('AdditionalDoc', 'integer', format='>9', initial='0',
                 label='Additional Documentation',
                 column_label='Additional Documentation')
        t.column('DelType', 'integer', format='>9',
                 label='Inv Delivery Type',
                 column_label='Inv Delivery Type')
        t.index('CustID', ['Brand', 'CustIdType', 'CustId'], area='Sta_Index_1')
        t.index('CustNum', ['CustNum', 'RowType', 'OrderId'], area='Sta_Index_1')
        t.index('OrderId', ['Brand', 'OrderId', 'RowType'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('OrderCustomer')


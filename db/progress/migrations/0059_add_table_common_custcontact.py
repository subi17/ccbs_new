from gearbox.migrations import Migration

class AddCustContact(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustContact', area='Sta_Data_32',
                       label='CustContact',
                       dump_name='custcontact',
                       desc='CustContact')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='CustContact',
                 column_label='CustContact',
                 help='CustContact\'s number')
        t.column('CustType', 'integer', mandatory=True, format='>>9', initial='0',
                 column_label='CustType',
                 help='Contact customer type')
        t.column('CustName', 'character', format='x(30)', initial='',
                 label='CustContact\'s name',
                 column_label='CustContact\'s name')
        t.column('Address', 'character', format='x(30)', initial='',
                 column_label='Address',
                 help='CustContact\'s mailing address (street, p.o. box)')
        t.column('ZipCode', 'character', initial='',
                 label='Postal code',
                 column_label='Postcd',
                 help='CustContact\'s postal code')
        t.column('PostOffice', 'character', format='x(24)', initial='',
                 label='Postal Addr.',
                 column_label='Postaddr',
                 help='CustContact\'s postal address')
        t.column('Country', 'character', format='x(3)', initial='',
                 column_label='Country',
                 help='CustContact\'s country code')
        t.column('Language', 'integer', format='>>9', initial='0',
                 column_label='Language',
                 help='CustContact\'s language code (1 ...999)',
                 valexp='Language > 0',
                 valmsg='Must be 1 ... 9 !')
        t.column('OrgId', 'character', format='x(11)', initial='',
                 label='Pers/Comp.ID',
                 help='CustContact\'s organisation ID or personal ID')
        t.column('Email', 'character', format='x(60)', initial='',
                 column_label='Email',
                 help='CustContact\'s Email ID')
        t.column('HonTitle', 'character', format='x(16)', initial='',
                 label='Title',
                 column_label='Title',
                 help='CustContact honorary title (printed f.ex. into invoice)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('SMSNumber', 'character', format='x(15)', initial='',
                 label='SMS Number',
                 column_label='SMS',
                 help='Mobile number for SMS messages')
        t.column('FirstName', 'character', format='x(20)', initial='',
                 label='Forename',
                 column_label='Forename',
                 help='CustContact\'s forename')
        t.column('DirMarkSMS', 'logical', initial='no',
                 label='Direct Marketing Via SMS',
                 column_label='Dir.Mark.SMS',
                 help='Direct marketing using SMS')
        t.column('DirMarkEmail', 'logical', initial='no',
                 label='Direct Marketing Via Email',
                 column_label='Dir.Mark.Email',
                 help='Direct marketing using Email')
        t.column('DirMarkPost', 'logical', initial='no',
                 label='Direct Marketing Via Post',
                 column_label='Dir.Mark.Post',
                 help='Direct marketing using post')
        t.column('OutMarkPost', 'logical', initial='no',
                 label='Out. Marketing Via Post',
                 column_label='Out.Mark.Post',
                 help='3rd part marketing using post')
        t.column('OutMarkSMS', 'logical', initial='no',
                 label='Out. Marketing Via SMS',
                 column_label='Out.Mark.SMS',
                 help='3rd party marketing using SMS')
        t.column('OutMarkEmail', 'logical', initial='no',
                 label='Out. Marketing Via Email',
                 column_label='Out.Mark.Email',
                 help='3rd party marketing using eMail')
        t.column('CustIdType', 'character', initial='',
                 label='CustContact ID Type',
                 column_label='ID Type',
                 help='CustContact ID type')
        t.column('Nationality', 'character', initial='',
                 column_label='Nation.')
        t.column('SurName2', 'character', format='x(30)', initial='',
                 label='Second Surname',
                 column_label='2.Surname',
                 help='Second surname')
        t.column('Region', 'character', initial='',
                 help='Region code')
        t.column('AddressCodC', 'character', initial='',
                 label='Address CodC',
                 column_label='CodC',
                 help='CodC in address validation')
        t.column('AddressCodP', 'character', initial='',
                 label='Address CodP',
                 column_label='CodP',
                 help='CodP in address validation')
        t.column('Phone', 'character', format='x(16)', initial='',
                 label='PhoneNo',
                 column_label='PhoneNo',
                 help='CustContact\'s phone number')
        t.index('CustContact', ['Brand', 'CustNum', 'CustType'], area='Sta_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CustContact')


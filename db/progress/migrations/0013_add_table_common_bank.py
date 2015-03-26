from gearbox.migrations import Migration

class AddBank(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Bank', area='Sta_Data_256',
                       label='xxBank',
                       dump_name='xxBank',
                       desc='xxBank')
        t.column('BankOffice', 'character', mandatory=True, format='x(4)', initial='',
                 label='Office',
                 column_label='Office',
                 help='Bank\'s name')
        t.column('ZipCode', 'character', format='x(5)', initial='',
                 label='Zip',
                 column_label='Zip',
                 help='ZIP code')
        t.column('City', 'character', format='x(16)', initial='',
                 column_label='City',
                 help='Location city')
        t.column('Address', 'character', format='x(20)', initial='',
                 label='Street',
                 column_label='Street',
                 help='Street address')
        t.column('BankId', 'character', format='X(4)', initial='',
                 column_label='BankId',
                 help='Identification code for bank')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Name', 'character', format='x(20)', initial='',
                 column_label='Name',
                 help='Bank\'s name')
        t.column('Name2', 'character', format='x(20)', initial='',
                 column_label='Name2',
                 help='Bank\'s name')
        t.column('FileDate', 'date', format='99-99-99',
                 label='File Date',
                 column_label='Date',
                 help='Date of the file from which data was read')
        t.index('BankId', ['Brand', 'BankId', 'BankOffice'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('Bank')


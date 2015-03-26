from gearbox.migrations import Migration

class AddCompany(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Company', area='Sta_Data_32',
                       label='Company',
                       dump_name='company',
                       desc='Company data')
        t.column('CompName', 'character', format='x(30)', initial='',
                 column_label='Comp.Name',
                 help='Company\'s name')
        t.column('Address', 'character', format='x(30)', initial='',
                 column_label='Address',
                 help='Company\'s address')
        t.column('PostOffice', 'character', format='x(30)', initial='',
                 label='Postcode',
                 column_label='Postcode',
                 help='Company\'s postal code + city')
        t.column('Phone', 'character', format='x(30)', initial='',
                 column_label='Phone',
                 help='Company\'s telephone number')
        t.column('Fax', 'character', format='x(20)', initial='',
                 column_label='Fax',
                 help='Company\'s fax number')
        t.column('UnitCode', 'integer', format='>>9', initial='0',
                 label='Unit',
                 help='Unit number')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Phone2', 'character', format='x(30)', initial='',
                 label='Phone 2',
                 help='Phone')
        t.column('Phone3', 'character', format='x(30)', initial='',
                 label='Phone 3',
                 help='Phone')
        t.column('CompanyID', 'character', format='x(20)', initial='',
                 label='Company ID')
        t.column('HomeLocation', 'character', format='x(30)', initial='',
                 label='Home Location',
                 column_label='Home',
                 help='Home location that is printed to invoice')
        t.column('Address2', 'character', format='x(30)', initial='',
                 label='Address 2',
                 column_label='Addr2',
                 help='Additional address')
        t.column('Address3', 'character', format='x(30)', initial='',
                 label='Address 3',
                 column_label='Addr3',
                 help='Additional address')
        t.column('Address4', 'character', format='x(30)', initial='',
                 label='Address 4',
                 column_label='Addr4',
                 help='Additional address')
        t.index('unitcode', ['Brand', 'UnitCode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Company')


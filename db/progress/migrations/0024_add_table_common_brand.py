from gearbox.migrations import Migration

class AddBRAND(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BRAND', area='Sta_Data_256',
                       dump_name='brand',
                       desc='Brand info\
')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('BRName', 'character', format='x(20)', initial='',
                 label='BrName',
                 column_label='BrName',
                 help='Name of Brand')
        t.column('directory', 'character', format='x(50)', initial='',
                 label='Dir',
                 column_label='Dir',
                 help='Directory Of Brand')
        t.column('Address', 'character', format='x(25)', initial='',
                 column_label='Addr.',
                 help='Address of Brand')
        t.column('Phone', 'character', format='x(18)', initial='',
                 label='Tel',
                 column_label='Tel',
                 help='Telefon Number')
        t.column('ContactName', 'character', format='x(20)', initial='',
                 column_label='ContactName',
                 help='Contact Name')
        t.column('email', 'character', format='x(50)', initial='',
                 label='Email',
                 column_label='Email')
        t.column('ProgPath', 'character', format='x(40)', initial='',
                 label='Program Path',
                 column_label='Path',
                 help='Brand specific program path (propath)')
        t.column('PostOffice', 'character', format='x(24)', initial='',
                 label='Postal Address',
                 column_label='Post Addr.',
                 help='Postal address')
        t.index('Brand', ['Brand'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('BRName', ['BRName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('BRAND')


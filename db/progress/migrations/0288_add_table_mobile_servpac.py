from gearbox.migrations import Migration

class AddServPac(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('ServPac', area='Sta_Data_128',
                       label='SerPac',
                       dump_name='servpac',
                       desc='\
')
        t.column('ServPac', 'character', format='x(12)', initial='',
                 label='Service Package',
                 column_label='ServPack',
                 help='Service Package Code')
        t.column('SPName', 'character', format='x(40)', initial='',
                 label='Service Pack Name',
                 column_label='Name',
                 help='Name of Service Package')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo of Service Package')
        t.column('FeeModel', 'character', format='x(12)', initial='',
                 column_label='FeeModel',
                 help='Possible Billing Event')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('ServiceLimit', 'character', format='x(16)', initial='',
                 label='Service Limit',
                 column_label='SLimit',
                 help='Service limit group')
        t.index('ServPac', ['Brand', 'ServPac'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('SPName', ['Brand', 'SPName', 'ServPac'], area='Sta_Index_3')

    def down(self):
        self.drop_table('ServPac')


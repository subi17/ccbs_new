from gearbox.migrations import Migration

class AddService(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('Service', area='Sta_Data_128',
                       label='Service',
                       dump_name='service',
                       desc='\
')
        t.column('Service', 'character', initial='',
                 label='Service Group',
                 column_label='Service',
                 help='Code of Service (group)')
        t.column('SEName', 'character', format='x(40)', initial='',
                 label='ServName',
                 column_label='Name Of Service',
                 help='Name of Service(group)')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo of Service')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('SEName', ['Brand', 'SEName', 'Service'], area='Sta_Index_3')
        t.index('Service', ['Brand', 'Service'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Service')


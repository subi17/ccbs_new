from gearbox.migrations import Migration

class AddSimMan(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SimMan', area='Sta_Data_128',
                       dump_name='simman',
                       desc='\
SIM manufacturer\
')
        t.column('Mancode', 'character', mandatory=True, initial='',
                 label='Manufacturer',
                 column_label='Manufacturer',
                 help='Code of Manufacturer')
        t.column('ManName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Manufacturer Name',
                 help='Name of Manufacturer')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('mancode', ['Brand', 'Mancode'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('manname', ['Brand', 'ManName', 'Mancode'], area='Sta_Index_3',
                unique=True)

    def down(self):
        self.drop_table('SimMan')


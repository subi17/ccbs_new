from gearbox.migrations import Migration

class AddServEl(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('ServEl', area='Sta_Data_256',
                       label='Servel',
                       dump_name='servel',
                       desc='Service elements (service component group)\
\
')
        t.column('ServPac', 'character', format='x(12)', initial='',
                 label='Service Package',
                 column_label='ServPac',
                 help='Code of ServPack')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Service Component',
                 column_label='Component',
                 help='Code of Service Component (a single service)')
        t.column('SeValue', 'integer', format='>>>9', initial='0',
                 label='Value',
                 column_label='Value',
                 help='Default Value for Service Parameter')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('ServCom', ['Brand', 'ServCom', 'ServPac'], area='Sta_Index_3',
                unique=True)
        t.index('ServPac', ['Brand', 'ServPac', 'ServCom'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ServEl')


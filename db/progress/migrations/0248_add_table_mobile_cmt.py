from gearbox.migrations import Migration

class AddCMT(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('CMT', area='Sta_Data_256',
                       dump_name='puhlaji')
        t.column('CMT', 'character', mandatory=True, format='x(2)', initial='',
                 label='Number',
                 column_label='Number')
        t.column('CMTName', 'character', format='x(30)', initial='',
                 label='Description',
                 column_label='Description')
        t.index('CMT', ['CMT'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CMT')


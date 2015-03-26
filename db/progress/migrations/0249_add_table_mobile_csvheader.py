from gearbox.migrations import Migration

class AddCSVHeader(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('CSVHeader', area='Sta_Data_32',
                       dump_name='csvheade')
        t.column('Version', 'character', format='x(6)', initial='',
                 column_label='Version')
        t.column('CSV', 'character', initial='',
                 column_label='CSV')
        t.index('Version', ['Version'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('CSVHeader')


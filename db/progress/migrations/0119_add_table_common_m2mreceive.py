from gearbox.migrations import Migration

class AddM2MReceive(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('M2MReceive', area='Sta_Data_32',
                       dump_name='m2mrecei')
        t.column('XMLMessage', 'character', initial='',
                 label='XML Message',
                 column_label='XML Message')
        t.column('RecStatus', 'integer', format='>9', initial='0',
                 label='Status',
                 column_label='Status')
        t.index('RecStatus', ['RecStatus'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('M2MReceive')


from gearbox.migrations import Migration

class AddMNPXml(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPXml', area='Sta_Data_256',
                       dump_name='mnpxml')
        t.column('XMLSeq', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='XMLSeq')
        t.column('RawMessage', 'clob', format='x(8)',
                 description='Raw mnp message',
                 area='Lob_Data', size='1M')
        t.index('XMLSeq', ['XMLSeq'], area='Dyn_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MNPXml')


from gearbox.migrations import Migration

class AddOFItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('OFItem', area='Sta_Data_256',
                       dump_name='ofitem')
        t.column('OFID', 'integer', format='>>>>>9', initial='0',
                 column_label='OFID')
        t.column('StatusCode', 'character', initial='',
                 column_label='StatudCode',
                 help='Order\'s statuscode')
        t.index('OFID', ['OFID'], area='Sta_Index_1',
                primary=True)
        t.index('StatusCode', ['StatusCode', 'OFID'], area='Sta_Index_1')

    def down(self):
        self.drop_table('OFItem')


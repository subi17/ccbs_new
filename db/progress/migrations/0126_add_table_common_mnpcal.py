from gearbox.migrations import Migration

class AddMNPCal(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPCal', area='Sta_Data_32',
                       dump_name='mnpcal')
        t.column('Region', 'character', format='x(12)', initial='')
        t.column('MessageType', 'character', initial='')
        t.column('Periods', 'integer', initial='0')
        t.column('OrderChannel', 'character', format='x(16)', initial='',
                 label='Order Channel',
                 column_label='Channel',
                 help='Order channel')
        t.index('MessageType', ['MessageType', 'Region'], area='Sta_Index_1')
        t.index('OrderChannel', ['OrderChannel', 'Region', 'MessageType'], area='Sta_Index_1')
        t.index('Region', ['Region', 'MessageType'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('MNPCal')


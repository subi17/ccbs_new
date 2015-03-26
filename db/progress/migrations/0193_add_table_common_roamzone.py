from gearbox.migrations import Migration

class AddRoamZone(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RoamZone', area='Sta_Data_128',
                       desc='Roaming Zone ')
        t.column('RoamZone', 'character', format='x(12)', initial='',
                 column_label='RoamZone')
        t.column('RZName', 'character', initial='',
                 label='RoamZone Name',
                 column_label='RoamZone Name',
                 help='Name of RoamZone')
        t.index('RoamZone', ['RoamZone'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RoamZone')


from gearbox.migrations import Migration

class AddRoamGroup(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('RoamGroup', area='Sta_Data_256',
                       dump_name='roamgrou')
        t.column('RoamGroup', 'character', initial='',
                 column_label='RoamGroup')
        t.column('Name', 'character', format='x(20)', initial='',
                 column_label='Name')
        t.index('RoamGroup', ['RoamGroup'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('RoamGroup')


from gearbox.migrations import Migration

class AddstarParamGroup(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('starParamGroup', area='Sta_Data_256',
                       dump_name='starprgr')
        t.column('groupCode', 'character', format='X(20)', initial='',
                 label='Group',
                 help='Group code')
        t.column('groupName', 'character', format='X(40)', initial='',
                 label='Name')
        t.index('GroupCode', ['groupCode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('starParamGroup')


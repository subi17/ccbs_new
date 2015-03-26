from gearbox.migrations import Migration

class AddStarSessionData(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('StarSessionData', area='Sta_Data_256',
                       dump_name='starsesd')
        t.column('SessionId', 'integer', format='->,>>>,>>>,>>9', initial='0',
                 column_label='Id')
        t.column('programname', 'character', format='X(50)', initial='',
                 label='Program name')
        t.column('data', 'character', format='X(78)', initial='',
                 label='Data',
                 help='Values using get/setStarProperty method')
        t.index('ProgramName', ['SessionId', 'programname'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('StarSessionData')


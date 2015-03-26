from gearbox.migrations import Migration

class Addjobcron(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('jobcron', area='Sta_Data_256')
        t.column('jobgroup', 'character', format='X(10)', initial='',
                 label='Job group',
                 help='Job group code')
        t.column('number', 'integer', format='>9', initial='0',
                 label='Number',
                 help='Job groups sequence number')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('InputData', 'character', format='X(256)', initial='',
                 label='Input Data',
                 help='starEntry list')
        t.index('number', ['jobgroup', 'number'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('programCode', ['jobgroup'], area='Sta_Index_2')

    def down(self):
        self.drop_table('jobcron')


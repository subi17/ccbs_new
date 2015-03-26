from gearbox.migrations import Migration

class AddstarSessionActivity(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('starSessionActivity', area='Sta_Data_256',
                       dump_name='starsesa')
        t.column('usercode', 'character', format='X(12)', initial='',
                 label='Usercode')
        t.column('clienttype', 'character', format='X(12)', initial='',
                 label='Client Type',
                 help='Client type')
        t.column('actionDate', 'date', format='99.99.99',
                 label='Action date')
        t.column('daycount', 'integer', format='>>>>>9', initial='0',
                 label='Day count',
                 help='Action count per day')
        t.column('hourcount', 'integer', extent=24, format='>>>>>9', initial='0',
                 label='Hour',
                 help='Action count per hour')
        t.index('actionDate', ['actionDate'], area='Sta_Index_2')
        t.index('clientType', ['clienttype', 'actionDate'], area='Sta_Index_2')
        t.index('userCode', ['usercode', 'clienttype', 'actionDate'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('starSessionActivity')


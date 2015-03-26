from gearbox.migrations import Migration

class AddlocalProgram(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('localProgram', area='Sta_Data_256',
                       dump_name='lprogram')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('rightgroup', 'character', format='X(12)', initial='',
                 label='RightGroup',
                 help='Right group code')
        t.column('PrintDestinations', 'character', format='X(40)', initial='',
                 label='Print destinations',
                 help='Available print destinations')
        t.column('emailCode', 'integer', format='9', initial='0',
                 label='eMail code',
                 description='0=all\
1=self\
2=to list\
3=not allowed')
        t.column('eMail', 'character', format='X(50)', initial='',
                 help='eMail addresses. Each address to own line')
        t.column('menutooltip', 'character', format='X(256)', initial='',
                 label='Tooltip',
                 help='Program tooltip. Shows on menus... local text')
        t.column('archive', 'character', format='X(10)', initial='',
                 label='Archive',
                 help='Archive time',
                 description='Default\
Year\
Month\
Week\
Permanent')
        t.index('programcode', ['programcode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('localProgram')


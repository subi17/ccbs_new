from gearbox.migrations import Migration

class Addmoreinfo(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('moreinfo', area='Sta_Data_256',
                       label='MoreInfo',
                       desc='Moreinfo menus')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('number', 'integer', format='>9', initial='0',
                 label='Number',
                 help='Number for moreinfo')
        t.column('name', 'character', format='X(40)', initial='',
                 label='Name',
                 help='Program name. Use menu labels and program title')
        t.column('makemenus', 'logical', initial='no',
                 label='Make menus')
        t.column('makebuttons', 'logical', initial='yes',
                 label='Make buttons')
        t.index('moreinfo', ['programcode', 'number'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('moreinfo')


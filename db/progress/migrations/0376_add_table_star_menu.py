from gearbox.migrations import Migration

class Addmenu(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('menu', area='Sta_Data_256',
                       label='Menu')
        t.column('applcode', 'character', mandatory=True, initial='',
                 label='Application',
                 help='Application code. Internal use only')
        t.column('menucode', 'character', initial='',
                 label='MenuCode',
                 help='MenuCode for menu')
        t.column('name', 'character', format='X(40)', initial='',
                 label='Name',
                 help='Menu name')
        t.column('version', 'character', initial='',
                 label='Version',
                 help='Version. Blank = "default"')
        t.column('rightgroup', 'character', format='X(12)', initial='',
                 label='RightGroup',
                 help='Right group code')
        t.column('visible', 'logical', initial='yes',
                 label='Visible',
                 help='If no, only programs can call this menu')
        t.column('menutooltip', 'character', format='X(256)', initial='',
                 label='Tooltip',
                 help='Menu tooltip.')
        t.column('sortField', 'character', format='X(12)', initial='',
                 label='SortField',
                 help='Sort order of menues')
        t.column('iconfile', 'character', format='X(40)', initial='',
                 label='Iconfile',
                 help='Icon shows in menues..')
        t.index('main', ['applcode', 'menucode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('menucode', ['menucode'], area='Sta_Index_2')
        t.index('name', ['name'], area='Sta_Index_2')
        t.index('sortField', ['sortField'], area='Sta_Index_2')

    def down(self):
        self.drop_table('menu')


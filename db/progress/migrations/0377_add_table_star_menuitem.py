from gearbox.migrations import Migration

class Addmenuitem(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('menuitem', area='Sta_Data_256',
                       label='MenuItem')
        t.column('applcode', 'character', mandatory=True, initial='',
                 label='Application',
                 help='Application code. Internal use only')
        t.column('menucode', 'character', initial='',
                 label='MenuCode',
                 help='MenuCode for menu')
        t.column('itemnumber', 'integer', format='>9', initial='0',
                 label='Number',
                 help='Itemnumber.')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('subapplcode', 'character', mandatory=True, initial='',
                 label='SubApplication',
                 help='Submenu application code.')
        t.column('submenucode', 'character', initial='',
                 label='SubMenuCode',
                 help='Menumenu code for menu')
        t.index('item', ['applcode', 'menucode', 'itemnumber'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('programcode', ['programcode'], area='Sta_Index_2')
        t.index('submenu', ['subapplcode', 'submenucode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('menuitem')


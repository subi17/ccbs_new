from gearbox.migrations import Migration

class Addusermenu(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('usermenu', area='Sta_Data_256',
                       label='User menus')
        t.column('usercode', 'character', initial='',
                 label='User',
                 help='User code')
        t.column('applcode', 'character', mandatory=True, initial='',
                 label='Application',
                 help='Application code. Internal use only')
        t.column('menucode', 'character', initial='',
                 label='MenuCode',
                 help='MenuCode for menu')
        t.column('positionx', 'integer', format='>>>>9', initial='0',
                 label='Pos-X',
                 help='Position x on the screen')
        t.column('positiony', 'integer', format='>>>>9', initial='0',
                 label='Pos-Y',
                 help='Position y on the screen')
        t.column('startup', 'logical', initial='no',
                 label='StartUp',
                 help='Menu will be start up automaticly on start')
        t.index('menucode', ['applcode', 'menucode'], area='Sta_Index_2')
        t.index('usercode', ['usercode', 'applcode', 'menucode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('usermenu')


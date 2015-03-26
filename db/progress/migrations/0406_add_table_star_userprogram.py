from gearbox.migrations import Migration

class Adduserprogram(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('userprogram', area='Sta_Data_256',
                       label='User programs',
                       dump_name='userprog',
                       desc='postition ?\
')
        t.column('usercode', 'character', initial='',
                 label='User',
                 help='User code')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('positionx', 'integer', format='>>>>9', initial='0',
                 label='Pos-X',
                 help='Position x on the screen')
        t.column('positiony', 'integer', format='>>>>9', initial='0',
                 label='Pos-Y',
                 help='Position y on the screen')
        t.column('paramstr', 'character', format='X(40)', initial='',
                 label='Parameters',
                 help='Parameters to program',
                 description='Program got these information in PRIVATE-DATA')
        t.index('userprogram', ['usercode', 'programcode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('userprogram')


from gearbox.migrations import Migration

class AddFColor(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FColor', area='Sta_Data_256',
                       label='Colours',
                       dump_name='fcolor',
                       desc='Colours')
        t.column('FrameName', 'character', mandatory=True, format='x(12)', initial='',
                 label='FrmName',
                 column_label='FrmName',
                 help='Frame\'s name in a code')
        t.column('TitleColor', 'character', format='x(24)', initial='',
                 label='Header\'s colour',
                 column_label='Header\'s colour',
                 help='Pair of colors for frame\'s title bar, fg/bg')
        t.column('FrameColor', 'character', format='x(24)', initial='',
                 label='FrColour',
                 column_label='FrColour',
                 help='"Colors of a frame, separated with slash ""/"""')
        t.index('FrameName', ['FrameName'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('FColor')


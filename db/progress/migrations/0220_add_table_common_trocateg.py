from gearbox.migrations import Migration

class AddTroCateg(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TroCateg', area='Sta_Data_256',
                       label='Trouble Category',
                       dump_name='trocateg',
                       desc='Trouble Category')
        t.column('TTCat', 'character', mandatory=True, initial='',
                 label='Trouble Category',
                 column_label='Trouble Category',
                 help='Trouble Category code')
        t.column('Memo', 'character', format='x(30)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 help='Brief description for the category defined',
                 description='Explanation')
        t.column('Handler', 'character', initial='',
                 label='Responsible Person',
                 column_label='Responsible Person',
                 help='Responsible person (handler) for this category of troubles',
                 description='Responsible Person')
        t.column('FgColor', 'integer', format='>>9', initial='0',
                 label='ForeGround Color',
                 column_label='FG Color')
        t.column('BgColor', 'integer', format='>>9', initial='0',
                 label='BackGround Color',
                 column_label='BG Color')
        t.column('Duration', 'decimal', format='>>9.99', initial='0.00',
                 column_label='Duration',
                 help='Default Category Duration')
        t.index('Handler', ['Handler'], area='Sta_Index_2')
        t.index('TTCat', ['TTCat'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TroCateg')


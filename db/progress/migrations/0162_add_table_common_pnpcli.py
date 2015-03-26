from gearbox.migrations import Migration

class AddPNPCLI(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PNPCLI', area='Sta_Data_64',
                       label='PNP CLI',
                       dump_name='pnpcli',
                       desc='PNP CLI\
')
        t.column('PNPGroup', 'character', format='x(10)', initial='',
                 label='PNP Group',
                 column_label='Group',
                 help='PNP group')
        t.column('MSSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='Subscr.')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Begin Date',
                 column_label='From',
                 help='Valid from')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='End Date',
                 column_label='To',
                 help='Valid to')
        t.column('PNPSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Sequence',
                 column_label='Seq',
                 help='PNP Sequence')
        t.index('MsSeq', ['MSSeq', 'PNPGroup', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('PNPGroup', ['PNPGroup', 'MSSeq', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')
        t.index('PNPSeq', ['PNPSeq'], area='Sta_Index_2')

    def down(self):
        self.drop_table('PNPCLI')


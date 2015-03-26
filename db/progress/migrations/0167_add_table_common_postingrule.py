from gearbox.migrations import Migration

class AddPostingRule(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PostingRule', area='Sta_Data_256',
                       label='Posting Rules',
                       dump_name='postingr',
                       desc='Posting rules')
        t.column('BIGroup', 'character', initial='',
                 label='Product group',
                 column_label='Prod.grp')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Effective date',
                 column_label='Eff.date',
                 help='Date when rule becomes effective')
        t.column('AccNum', 'integer', format='>>>>>9', initial='0',
                 label='Account',
                 column_label='Acc',
                 help='Account number')
        t.column('UnbAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Acc. for unb.',
                 column_label='Unb.Acc',
                 help='Account number for unbilled events')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Account', ['Brand', 'AccNum'], area='Sta_Index_2')
        t.index('FromDate', ['Brand', ('FromDate', 'DESCENDING'), 'BIGroup'], area='Sta_Index_2')
        t.index('Pgcode', ['Brand', 'BIGroup', ('FromDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PostingRule')


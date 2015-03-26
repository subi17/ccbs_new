from gearbox.migrations import Migration

class AddClaimHist(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ClaimHist', area='Sta_Data_128',
                       label='Claiming history',
                       dump_name='claimhis',
                       desc='Claiming history of invoice')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Invoice Number')
        t.column('ClaimDate', 'date', format='99-99-99',
                 label='Claiming Date',
                 column_label='Date',
                 help='Claiming date')
        t.column('Claim', 'integer', format='>>9', initial='0',
                 label='Claim nbr',
                 column_label='Claim',
                 help='Claim number',
                 description='How many times have been claimed')
        t.column('Memo', 'character', format='x(30)', initial='',
                 label='Info')
        t.column('Handler', 'character', format='x(20)', initial='',
                 help='User id of the claim event handler')
        t.column('ClaimAmt', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Claimed Amount',
                 column_label='Amount',
                 help='Claimed amount (invoice\'s open balance)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('ClaimState', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Claiming Status',
                 column_label='Claimed',
                 help='Claiming status')
        t.index('ClaimDate', ['Brand', ('ClaimDate', 'DESCENDING'), 'InvNum'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', ('ClaimDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('InvNum', ['Brand', 'InvNum', 'Claim'], area='Sta_Index_2')
        t.index('InvNum_s', ['InvNum', 'Claim'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ClaimHist')


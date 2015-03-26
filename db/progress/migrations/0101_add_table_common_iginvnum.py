from gearbox.migrations import Migration

class AddIGInvNum(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('IGInvNum', area='Sta_Data_256',
                       label='InvGroup Invoice Nbr',
                       dump_name='iginvnum',
                       desc='Invoice group invoice number sequences\
')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('InvGroup', 'character', initial='',
                 help='Invoice group')
        t.column('InvType', 'integer', format='>9', initial='0',
                 label='Invoice Type',
                 column_label='Type',
                 help='Invoice type')
        t.column('InvNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Invoice Number',
                 column_label='Inv.Nbr',
                 help='Invoice number sequence (last used number)')
        t.column('SeqPrefix', 'character', initial='',
                 label='Sequence Prefix',
                 column_label='Prefix',
                 help='Sequence prefix')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when sequence becomes effective')
        t.index('InvGroup', ['Brand', 'InvGroup', 'InvType', ('FromDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('IGInvNum')


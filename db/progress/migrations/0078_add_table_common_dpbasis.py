from gearbox.migrations import Migration

class AddDPBasis(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DPBasis', area='Sta_Data_256',
                       label='Volume Discount Basis',
                       dump_name='dpbasis',
                       desc='Basis of volume discount (DpConf).\
')
        t.column('DPConfNum', 'integer', format='->>>>>>9', initial='0',
                 label='Sequence',
                 column_label='Seq',
                 help='Unique sequence nbr for DpConf')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Product',
                 column_label='Product',
                 help='Product code')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Call Case Number for call\'s destination')
        t.column('BasisType', 'integer', format='>9', initial='0',
                 label='Basis Type',
                 column_label='Type',
                 help='Type of the Discount Basis',
                 description='0=Generic, 1=Product, 2=CCN, 3=bSub')
        t.column('KeyField', 'character', format='x(16)', initial='',
                 column_label='KeyField',
                 help='Keyfield')
        t.index('DpConfNum', ['DPConfNum', ('BasisType', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('KeyField', ['DPConfNum', 'KeyField'], area='Sta_Index_2')

    def down(self):
        self.drop_table('DPBasis')


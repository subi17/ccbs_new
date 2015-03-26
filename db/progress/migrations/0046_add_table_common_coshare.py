from gearbox.migrations import Migration

class AddCoShare(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CoShare', area='Sta_Data_256',
                       dump_name='coshare')
        t.column('TargType', 'character', format='X(1)', initial='',
                 label='Target Type',
                 column_label='Target Type',
                 help='Commission Target Type. Valid values are C, S or R',
                 description='Valid values are (C)ustomer, (S)alesman or (R)eseller')
        t.column('CoTarg', 'character', format='X(10)', initial='',
                 label='Commission Target',
                 column_label='Commission Target',
                 help='Commission Target Code based on Target type')
        t.column('RsLevel', 'integer', format='>9', initial='0',
                 label='Reseller Level',
                 column_label='RSLevel',
                 help='Salesman\'s level in reseller\'s organization')
        t.column('CoPerc', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Share Percentage',
                 column_label='Share%',
                 help='Percentage of the commission amount (target\'s share)')
        t.column('CoAmt', 'decimal', decimals=2, initial='0',
                 label='Share Amount',
                 column_label='Amount',
                 help='Fixed share of the commission amount')
        t.column('CoTargId', 'integer', format='>>>>>>>9', initial='0',
                 label='Target ID',
                 column_label='TargID',
                 help='Commission target ID')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CoTarg', ['Brand', 'TargType', 'CoTarg'], area='Sta_Index_2')
        t.index('CoTargId', ['CoTargId', 'TargType', 'CoTarg', 'RsLevel'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('CoShare')


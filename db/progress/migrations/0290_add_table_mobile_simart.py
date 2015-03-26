from gearbox.migrations import Migration

class AddSimArt(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SimArt', area='Sta_Data_128',
                       dump_name='simart',
                       desc='SIM Article\
')
        t.column('SimArt', 'character', format='x(12)', initial='',
                 label='Sim Article',
                 column_label='Sim Article',
                 help='Article Code for a SIM type')
        t.column('ManCode', 'character', initial='',
                 label='Manufacturer',
                 column_label='Manufacturer',
                 help='Code Of SIM Manufacturer')
        t.column('SAName', 'character', format='x(40)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of a SIM Card Type')
        t.column('Memo', 'character', format='x(60)', initial='',
                 column_label='Memo')
        t.column('Balance', 'integer', initial='0',
                 label='Total Balance',
                 column_label='Total Bal',
                 help='Toltal Balance (No. of individual cards in all stock locs)')
        t.column('OrdPoint', 'integer', format='>,>>>,>>9', initial='0',
                 label='OrderPoint',
                 column_label='OrderPoint',
                 help='Ordering Point (total)')
        t.column('DetBal', 'integer', extent=10, format='>,>>>,>>9', initial='0',
                 label='Balance',
                 column_label='Balance',
                 help='Number of cards')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('mancode', ['Brand', 'ManCode', 'SimArt'], area='Sta_Index_3',
                unique=True)
        t.index('SAName', ['Brand', 'SAName', 'SimArt'], area='Sta_Index_3',
                unique=True)
        t.index('SimArt', ['Brand', 'SimArt'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('SimArt')


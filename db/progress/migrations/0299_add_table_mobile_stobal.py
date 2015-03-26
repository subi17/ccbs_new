from gearbox.migrations import Migration

class AddStoBal(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('StoBal', area='Sta_Data_128',
                       dump_name='stobal',
                       desc='Article\'s Balance in certain stock\
')
        t.column('Stobal', 'character', initial='',
                 label='Stock',
                 column_label='Stock',
                 help='Stock Code')
        t.column('SimArt', 'character', format='x(12)', initial='',
                 label='Sim Article',
                 column_label='Sim Article',
                 help='Article Code for a SIM type')
        t.column('DetBal', 'integer', extent=10, format='>,>>>,>>9', initial='0',
                 label='Balance',
                 column_label='Balance')
        t.column('Balance', 'integer', initial='0',
                 label='TotalBal',
                 column_label='Total Bal',
                 help='Total Balance of article in this stock')
        t.column('OrdPoint', 'integer', format='>,>>>,>>9', initial='0',
                 label='OrderPoint',
                 column_label='OrderPoint',
                 help='Ordering Point')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('SimArt', ['Brand', 'SimArt', 'Stobal'], area='Sta_Index_3',
                unique=True)
        t.index('Stobal', ['Brand', 'Stobal', 'SimArt'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('StoBal')


from gearbox.migrations import Migration

class AddStock(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('Stock', area='Sta_Data_64',
                       dump_name='stock',
                       desc='Stock Location\
')
        t.column('Stock', 'character', initial='',
                 column_label='Stock',
                 help='Stock Code')
        t.column('StoName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name Of Stock')
        t.column('StoStreet', 'character', format='x(30)', initial='',
                 label='Street',
                 column_label='Street',
                 help='Street Address')
        t.column('StoCity', 'character', format='x(30)', initial='',
                 label='City',
                 column_label='City',
                 help='City (Postal no + Post Office)')
        t.column('Reseller', 'character', initial='',
                 label='Retailer',
                 column_label='Retailer',
                 help='An unique code for a retailer; maximum 8 characters')
        t.column('StoFile1', 'character', format='x(60)', initial='',
                 label='ICCFileName',
                 column_label='ICCFileName',
                 help='Directory and Name of \'ICC information\' output file')
        t.column('StoFile2', 'character', format='x(60)', initial='',
                 label='SubsDataFile',
                 column_label='SubsDataFile',
                 help='Directory and name of \'Subscriber Data\' output file')
        t.column('StoFile3', 'character', format='x(60)', initial='',
                 label='SIMDistFile',
                 column_label='SIMDistFile',
                 help='Directory and name of \'SIM Distribution\' output file')
        t.column('SimDel', 'integer', format='9', initial='0',
                 column_label='SimDel',
                 help='Default SIM Delivery Method',
                 valexp='SimDel < 3',
                 valmsg='0:External delivery file used 1:Low Prioirity 2:High Priority')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('StoType', 'character', initial='',
                 label='Type',
                 column_label='Type',
                 help='Stock Type')
        t.column('ZipCodeExp', 'character', initial='',
                 column_label='ZipCodeExp',
                 help='Zip code regular expression')
        t.index('Reseller', ['Brand', 'Reseller', 'Stock'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('Stock', ['Brand', 'Stock'], area='Sta_Index_3',
                unique=True)
        t.index('StoName', ['Brand', 'StoName', 'Stock'], area='Sta_Index_3',
                unique=True)

    def down(self):
        self.drop_table('Stock')


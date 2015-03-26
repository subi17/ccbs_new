from gearbox.migrations import Migration

class AddIFiSpx(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('IFiSpx', area='Sta_Data_256',
                       dump_name='ifispx',
                       desc='IMSI Delivery File Specifications\
')
        t.column('ManCode', 'character', initial='',
                 label='Manufact',
                 column_label='Manufact',
                 help='Code of Manufacturer')
        t.column('Version', 'character', format='x(12)', initial='',
                 column_label='Version',
                 help='Version Number of the file')
        t.column('Hrowd', 'integer', format='z9', initial='0',
                 label='HRows',
                 column_label='HR',
                 help='Number of Header Rows before Data rows')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo')
        t.column('ICC', 'integer', extent=2, format='zz9', initial='0',
                 column_label='ICC',
                 help='Position of ICC Code')
        t.column('IMSI', 'integer', extent=2, format='zz9', initial='0',
                 column_label='IMSI',
                 help='Position of IMSI Code')
        t.column('IsCode1', 'integer', extent=2, format='zz9', initial='0',
                 label='PIN1',
                 column_label='PIN1',
                 help='Position of PIN Code 1')
        t.column('IsCode2', 'integer', extent=2, format='zz9', initial='0',
                 label='PIN CODE2',
                 column_label='PIN CODE2',
                 help='Position of PIN Code 2')
        t.column('IsUnb1', 'integer', extent=2, format='zz9', initial='0',
                 label='UNB1',
                 column_label='UNB1',
                 help='Position of Unblock Code 1')
        t.column('IsUnb2', 'integer', extent=2, format='zz9', initial='0',
                 label='UNB2',
                 column_label='UNB2',
                 help='Position of Unblock Code 2')
        t.column('Ki', 'integer', extent=2, format='zz9', initial='0',
                 column_label='Ki',
                 help='Position of Ki Code')
        t.column('SimArt', 'character', format='x(12)', initial='',
                 label='SimArtCode',
                 column_label='SimArtCode',
                 help='Article Code for a SIM type')
        t.column('Isc1', 'integer', extent=2, format='zz9', initial='0',
                 label='ISC1',
                 column_label='ISC1',
                 help='Position of ISC1 Code')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('mancode', ['Brand', 'ManCode', 'Version'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('IFiSpx')


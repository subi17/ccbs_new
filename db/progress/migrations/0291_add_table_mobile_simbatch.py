from gearbox.migrations import Migration

class AddSimBatch(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SimBatch', area='Sta_Data_128',
                       dump_name='simbatch',
                       desc='\
')
        t.column('SimBatch', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Batch#',
                 column_label='Batch#',
                 help='Sequence for a Simbatch')
        t.column('ManCode', 'character', initial='',
                 label='Manufacturer',
                 column_label='Manufacturer',
                 help='Code of Manufacturer')
        t.column('SimArt', 'character', format='x(12)', initial='',
                 label='Sim Article',
                 column_label='Sim Article',
                 help='Article Code for a SIM type')
        t.column('TpKey', 'character', initial='',
                 label='TransKey',
                 column_label='TransKey',
                 help='Transport key')
        t.column('DelDate', 'date', format='99-99-99',
                 label='Delivery Date',
                 column_label='Delivery Date',
                 help='Day of Delivery')
        t.column('memo', 'character', extent=10, format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo',
                 help='Memo of ServPack')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('FileName', 'character', format='X(20)',
                 column_label='FileName',
                 help='Name of the read sim file')
        t.index('FileName', ['Brand', 'ManCode', 'FileName'], area='Sta_Index_3')
        t.index('ManCode', ['Brand', 'ManCode', 'DelDate'], area='Sta_Index_3',
                primary=True)
        t.index('SimArt', ['Brand', 'SimArt', 'DelDate'], area='Sta_Index_3')
        t.index('SimBatch', ['Brand', 'SimBatch'], area='Sta_Index_3',
                unique=True)

    def down(self):
        self.drop_table('SimBatch')


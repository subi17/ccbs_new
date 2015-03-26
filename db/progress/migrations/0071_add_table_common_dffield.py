from gearbox.migrations import Migration

class AddDFField(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DFField', area='Sta_Data_128',
                       label='Dump File Field',
                       dump_name='dffield',
                       desc='Fields in a dump file\
')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('DumpID', 'integer', format='>>>>>>>9', initial='0',
                 label='Dump ID',
                 column_label='ID',
                 help='Unique ID')
        t.column('DFTable', 'character', format='x(20)', initial='',
                 label='Table Name',
                 column_label='Table',
                 help='Table name')
        t.column('DFField', 'character', format='x(20)', initial='',
                 label='Field Name',
                 column_label='Field',
                 help='Field name')
        t.column('DFLabel', 'character', format='x(30)', initial='',
                 label='Label')
        t.column('OrderNbr', 'integer', format='>>>9', initial='0',
                 label='Order Number',
                 column_label='Order',
                 help='Order number in file')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.index('DumpID', ['DumpID', 'OrderNbr'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('DFField')


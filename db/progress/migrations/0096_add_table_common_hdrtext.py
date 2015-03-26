from gearbox.migrations import Migration

class AddHdrText(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('HdrText', area='Sta_Data_128',
                       label='HdrText',
                       dump_name='hdrtext',
                       desc='Header texts for reports')
        t.column('te-nro', 'integer', format='zz9', initial='0',
                 label='No',
                 column_label='No.',
                 help='Serial number(1 - 999) of a text element')
        t.column('te-kie', 'integer', mandatory=True, format='9', initial='0',
                 label='Lang',
                 column_label='Lang',
                 help='Language code (1 - 9)')
        t.column('te-text', 'character', format='x(60)', initial='',
                 label='Text',
                 column_label='Text',
                 help='Text in choosen language')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('te-kie', ['Brand', 'te-kie', 'te-nro'], area='Sta_Index_2',
                unique=True)
        t.index('te-nro', ['Brand', 'te-nro', 'te-kie'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('te-text', ['Brand', 'te-text'], area='Sta_Index_2')

    def down(self):
        self.drop_table('HdrText')


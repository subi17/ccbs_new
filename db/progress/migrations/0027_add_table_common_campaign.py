from gearbox.migrations import Migration

class AddCampaign(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Campaign', area='Sta_Data_128',
                       label='Campaign',
                       dump_name='campaign',
                       desc='Campaign header, rows in CampRow\
\
')
        t.column('Campaign', 'character', initial='',
                 label='Campaign ID',
                 column_label='ID')
        t.column('CaName', 'character', format='x(30)', initial='',
                 label='Campaign Name',
                 column_label='Name',
                 help='Campaign name')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Beginning of campaign')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='End of campaign')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('CampType', 'integer', format='9', initial='0',
                 label='Campaign Type',
                 column_label='Type',
                 help='Campaign type')
        t.column('AccessSchemaSeq', 'integer', initial='0',
                 column_label='AccessSchemaSeq',
                 description='Link to the Access Schema for this campaign')
        t.index('Campaign', ['Brand', 'Campaign'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CaName', ['Brand', 'CaName'], area='Sta_Index_2')
        t.index('ToDate', ['Brand', ('ToDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('Campaign')


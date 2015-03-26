from gearbox.migrations import Migration

class AddReportConfRow(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ReportConfRow', area='Sta_Data_256',
                       label='Report Configuration Row',
                       dump_name='reportconfrow',
                       desc='Report configuration row')
        t.column('Brand', 'character', initial='',
                 help='Code of Brand')
        t.column('ReportID', 'character', format='x(12)', initial='',
                 label='Report ID',
                 column_label='ID')
        t.column('RowType', 'character', initial='',
                 label='Row Type',
                 column_label='Type',
                 help='Row type')
        t.column('CharValue', 'character', format='x(12)', initial='',
                 label='Character Value',
                 column_label='Char',
                 help='Character value')
        t.column('DateValue', 'date', format='99-99-99',
                 label='Date Value',
                 column_label='Date',
                 help='Date value')
        t.column('IntValue', 'integer', format='->>>>>>>9', initial='0',
                 label='Integer Value',
                 column_label='Int',
                 help='Integer value')
        t.column('DecValue', 'decimal', decimals=6, format='->>>>>>>>9.9999', initial='0',
                 label='Decimal Value',
                 column_label='Dec',
                 help='Decimal value')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when configuration becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when configuration expires')
        t.column('ConfRowID', 'integer', format='>>>>>>>>>>9', initial='0',
                 label='Row ID',
                 column_label='ID',
                 help='Unique row ID')
        t.column('LogicValue', 'logical', initial='no',
                 label='Logical Value',
                 column_label='Logic',
                 help='Logical value')
        t.index('ConfRowID', ['ConfRowID'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ReportID', ['Brand', 'ReportID', 'RowType', ('ToDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('ReportConfRow')

